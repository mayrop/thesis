get_tune_grid <- function(model, type) {
  if (type == "glmnet") {
    return(expand.grid(
      as.list(model$finalModel$tuneValue)
    ))
  }
  
  if (type == "rf") {
    return(expand.grid(
      as.list(model$bestTune)
    ))
  }
  
  warning(paste("No valid function for type: ", type))
}

get_my_model <- function(method, data, control, family="binomial", metric="ROC", preProc=c("center", "scale")) {
  # ifelse is vectorized, so use `if`
  # https://stackoverflow.com/questions/1335830/why-cant-rs-ifelse-statements-return-vectors#answer-56915974
  my_list = list(
    form=data$form,
    data=data$data,
    method=`if`(!is.null(data$method), data$method, method),  
    family=`if`(!is.null(data$family), data$family, family),
    metric=`if`(!is.null(data$metric), data$metric, metric),
    trControl=`if`(!is.null(data$control), data$control, control),
    preProc=`if`(is.array(data$preProc), data$preProc, preProc),
    tuneGrid=`if`(!is.null(data$tuneGrid), data$tuneGrid, c())
  )
  
  params=`if`(is.list(data$params), data$params, list())  
  
  set.seed(config$seed)
  
  # multithreading....
  # TODO - check how many clusters to run
  cl <- makeCluster(3)
  registerDoParallel(cl)
  
  # https://www.r-bloggers.com/a-new-r-trick-for-me-at-least/
  my_model <- do.call('train', as.list(c(my_list, params)))
  
  stopCluster(cl)
  registerDoSEQ()
  rm(cl)
  
  return(my_model)
}
