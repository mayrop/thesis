##############################################
# L o a d i n g . h a z e l . p a c k a g e...
#
# https://github.com/mayrop/hazel
if (!require("hazel")) {
  print("Seems hazel package is not installed...")
  if (!require("devtools", character.only = TRUE)) {
    print("Seems devtools package is not installed... installing it now")
    install.packages("devtools")
  }
  
  library(devtools)
  print("Installing hazel package from github")
  devtools::install_github("mayrop/hazel")
}

###################################
# L o a d i n g . d a t a s e t s
#
print("Loading libraries")
load.libraries(config$settings$libraries)

print("Loading libraries from source")
load.sources(config$settings$sources)

print("Loading libraries from github")
load.github(config$settings$github)

print("Loading datasets")
datasets <- load.datasets(config$settings$data_folder)

###################################

############################################
# Functions that will later be used...

print("Defining functions...")

execute_and_plot <- function(file, 
                             save = FALSE, 
                             prefix = "", 
                             suffix = "",
                             ext = ".png", 
                             bg = "white", 
                             unit = "mm", 
                             res = 300, ...) {
  filename <- gsub("/", "_", file)
  filename <- gsub("^_", "", filename)
  filename <- gsub("\\.\\w+$", "", filename)  
  filename <- paste(prefix, filename, suffix, ext, sep="")
  
  # Only saving as PNG when we asked for it
  save & png(filename = filename, bg = bg, unit = unit, res = res, ...)
  source(file, print.eval = save)
  
  # Only turnning off dev environment if we asked for it
  save & dev.off()
}


# Based on http://jaehyeon-kim.github.io/2015/05/Setup-Random-Seeds-on-Caret-Package.html
get_seeds <- function(method="cv", n_resampling=10, n_tunes=0, seed=2019) {
  set.seed(seed)
  seeds <- vector(mode = "list", length = n_resampling)
  seeds <- lapply(seeds, function(x) {
    sample.int(n = 100000, size = n_resampling + n_tunes)
  })
  seeds[[length(seeds) + 1]] <- sample.int(n = 100000, size = 1)
  
  return(seeds)
}

#
# Redefening the twoClassSummary function from caret
# This is due that caret hardcodes the positive and negative when calculating
# Sensitivity and Specificity
#
two_class_summary <- function (data, lev = NULL, model = NULL, positive = "yes", negative = "no") {
  lvls <- levels(data$obs)

  # add the namespace for caret
  caret:::requireNamespaceQuietStop("ModelMetrics")
  
  # True positives
  tp <- sum(data[, "obs"] == positive & data[, "pred"] == positive)
  
  # True negatives
  tn <- sum(data[, "obs"] == negative & data[, "pred"] == negative)
  
  # False positives
  fp <- sum(data[, "obs"] == negative & data[, "pred"] == positive)
  
  # False negatives
  fn <- sum(data[, "obs"] == positive & data[, "pred"] == negative)
  
  # Redefine the metrics
  out = c(
    (tp + tn) / (tp + fp + tn + fn),
    tp / (tp + fn),
    tn / (tn + fp),
    tp / (tp + fp),
    tn / (tn + fn),
    ModelMetrics::auc(ifelse(data$obs == lev[1], 0, 1), data[, lvls[2]])
  )
  
  names(out) <- c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "AUC")

  return(out)

  # Removing caret functions
  # posPredValue(data[, "pred"], data[, "obs"], positive = positive)
  # negPredValue(data[, "pred"], data[, "obs"], negative = negative)
  # sensitivity(data[, "pred"], data[, "obs"], positive = positive), 
  # specificity(data[, "pred"], data[, "obs"], negative = negative),
  # as.numeric(unlist(e1071::classAgreement(table(data[, "obs"], data[, "pred"])))[c("kappa")])
}


#
# Function to evaluate a model
# Gives all the metrics ready to be summarized
#
model_evaluate <- function(model, data, response, levels, positive = "yes") {
  evaluation <- list()
  evaluation$probs <- predict(model, newdata = data, type = "prob")
  evaluation$raw <- predict(model, newdata = data, type = "raw")
  
  evaluation$roc <- roc(
    response = data[[response]],
    predictor = evaluation$probs[, positive],
    levels = levels
  )  
  
  evaluation$matrix <- confusionMatrix(
    data = evaluation$raw, 
    reference = data$response_factor,
    positive = positive
  )
  
  evaluation$metrics <- c(
    evaluation$matrix$byClass, 
    as.list(evaluation$matrix$overall), 
    list(AUC = evaluation$roc$auc)
  )
  
  # Removing spaces
  names(evaluation$metrics) <- sapply(names(evaluation$metrics), function(name) {
    return(gsub(" ", "", name))
  })
  
  return(evaluation)
}


#
# Gives bin for empirical logit plots
#
emplogit <- function(df, var = "x", response = "y", bins = 100) {
  labels <- (bins-1):0
  
  df$rank <- rank(-pull(df[,var]))
  df$bin <- labels[as.numeric(cut(df$rank, breaks = bins, labels = 0:(bins-1)))]
  
  return(
    df %>% 
      group_by(bin) %>% 
      summarise(
        freq = n(),
        successes = sum(!!rlang::sym(response)),
        var := mean(!!rlang::sym(var))
      ) %>% 
      mutate(
        elogit = log((successes + (sqrt(freq) / 2)) / (freq - successes + (sqrt(freq) / 2)))
      ) %>% 
      arrange(
        bin
      )
  )
}

eval.mcnemar <- function(response, 
                         model1, 
                         model2, 
                         names = c("Model 1", "Model 2"), 
                         headings = c("Correct", "Incorrect"),
                         correct = TRUE) {

  vals <- list(
    # both correct
    a = sum(response == model1 & response == model2),
    
    # only model 1 correct
    b = sum(response == model1 & response != model2),
    
    # only model 2 correct
    c = sum(response != model1 & response == model2),
    
    # both incorrect
    d = sum(response != model1 & response != model2)
  )

  my_dimnames <- list()
  my_dimnames[[names[1]]] = headings
  my_dimnames[[names[2]]] = headings
    
  mcnemar_table <- matrix(
    rbind(c(vals$a, vals$b), c(vals$c, vals$d)),
    nrow = 2,
    dimnames = my_dimnames
  )
  
  return(
    list(
      # redundant with mcnemar.table, but useful
      "classifications" = as.data.frame(vals),
      "mcnemar.table" = mcnemar_table,
      "mcnemar.test" = mcnemar.test(mcnemar_table, correct = correct)
    )
  )  
}

eval.cochrans_q <- function(y_target, x) {
  # k will be the number of classifiers
  k <- ncol(x)
  
  # changing colnames for convenience
  colnames(x) <- 1:k
  
  # calculate G
  G <- rep(0, length.out = k)
  
  for (i in 1:k) {
    G[i] <- sum(y_target == x[, i])
  }
  
  x$y_target <- y_target
  
  correct <- apply(x[,], 1, function(row) {
    # here we count how many classifiers equal the true class
    count_if(row[length(row)], row[-length(row)])
  })
  
  #Q <- (k-1) * (k * sum(G**2) - sum(G)**2) / (k*sum(G) - sum(correct**2))
  method <- "Cochran's Q test for comparing the performance of multiple classifiers"
  parameter <- k-1
  statistic <- (k-1) * (k * sum((G - mean(G))**2)) / (k*sum(G) - sum(correct**2))
  p <- pchisq(statistic, df = parameter, lower.tail = FALSE)
  
  names(statistic) <- "Cochran's Q chi-squared"
  names(parameter) <- "df"
  
  rval <- list(
    statistic = statistic, 
    parameter = parameter, 
    p.value = p, 
    method = method, 
    data.name = "data",
    alternate = ""
  )
  
  class(rval) <- "htest"
  return(rval)
}

