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



