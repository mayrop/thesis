to_run=names(my_methods)
to_run=c("glm")

for (method in to_run) {
  ################################## 
  control$allowParallel <- `if`(!is.null(my_methods[[method]]$allowParallel), my_methods[[method]]$allowParallel, FALSE)
  
  print(paste("Running", method))
  start_time <- Sys.time()
  
  # defaulting the formula
  my_methods[[method]]$form = `if`(!is.null(my_methods[[method]]$form), my_methods[[method]]$form, my_formula)
  my_methods[[method]]$data = `if`(!is.null(my_methods[[method]]$data), my_methods[[method]]$data, train.data)
  
  tunes = `if`(!is.null(my_methods[[method]]$tunes), my_methods[[method]]$tunes, 0)
  
  control$seeds <- get_seeds(n_tunes=tunes)
  
  my_models[[method]] <- get_my_model(method, my_methods[[method]], control=control)
  
  my_models[[method]]$benchmarks <- list(
    start=start_time,
    end=Sys.time()
  )
  
  # Get the optimized parameters
  if (!is.null(my_methods[[method]]$optimizeGrid)) {
    start_time <- Sys.time()
    
    my_methods[[method]]$tuneGrid = get_tune_grid(my_models[[method]], my_methods[[method]]$optimizeGrid)
    print("New optimized grid...")
    print(my_methods[[method]]$tuneGrid)
    my_methods[[method]]$optimizeGrid = NULL
    
    print("Running again...")
    my_models[[method]] = get_my_model(method, my_methods[[method]], control=control)
    
    my_models[[method]]$benchmarks$tuning <- list(
      start=start_time,
      end=Sys.time()
    )
  }
  
  ##################################
  # Evaluating on test data
  
  post <- list()
  
  post$probs <- predict(my_models[[method]], newdata=test.data,  type="prob")
  post$raw <- predict(my_models[[method]], newdata=test.data,  type="raw")
  post$roc <- roc(
    response = test.data$response_factor,
    predictor = post$probs[, "yes"],
    levels = levels(test.data$response_factor)
  )
  post$matrix <- confusionMatrix(
    data=post$raw, reference=test.data$response_factor, positive="yes"
  )
  
  post$metrics <- c(
    post$matrix$byClass, 
    as.list(post$matrix$overall), 
    list(AUC=post$roc$auc)
  )
  
  # Removing spaces
  names(post$metrics) <- sapply(names(post$metrics), function(name) {
    return(gsub(" ", "", name))
  })
  
  my_models[[method]]$post <- post
  
  ##################
  my_name <- my_methods[[method]]$name
  my_resamples[[my_name]] <- my_models[[method]]
  
  ################
  my_metrics[[method]] <- c(
    name=my_name,
    post$metrics
  )
}

