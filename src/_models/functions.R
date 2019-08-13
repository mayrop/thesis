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
  if (length(lvls) > 2) {
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  }

  # add the namespace for caret
  caret:::requireNamespaceQuietStop("ModelMetrics")
  
  if (!all(levels(data[, "pred"]) == lvls)) {
    stop("levels of observed and predicted data do not match")
  }

  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[1], 0, 1), data[, lvls[2]])

  # Redefine the metrics
  out <- c(
    rocAUC,
    sensitivity(data[, "pred"], data[, "obs"], positive = positive), 
    specificity(data[, "pred"], data[, "obs"], negative = negative)
  )

  # changing name from ROC to AUC
  names(out) <- c("AUC", "Sens", "Spec")
  out
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
    reference = test.data$response_factor,
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