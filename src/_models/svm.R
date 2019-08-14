################################## 
# S V M . R a d i a l . K e r n e l

print("Running SVM Radial Kernel...")

control <- base_control
control$seeds <- get_seeds(seed = config$seed, n_tunes = 38)
control$allowParallel <- TRUE
start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["svm_tuning"]] <- train(
  form = my_formula,
  data = train.data,
  method = "svmRadial",  
  family = "binomial",
  metric = "AUC",
  preProcess = c("center","scale"),
  trControl = control,
  tuneGrid = expand.grid(
    C = c(0.1, .5, 1, 10),
    sigma = sort(c(2^c(-15,-10, -5, -3), 0.05))
  )
)

my_models[["svm_tuning"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

# training with the best tune...
print("Now training with the best tune...")
start_time <- Sys.time()
# Resseting seed...
set.seed(config$seed)

# Now training with the tuned hyper parameters
my_models[["svm"]] <- train(
  form = my_formula,
  data = train.data,
  method = "svmRadial",  
  family = "binomial",
  metric = "AUC",
  preProcess = c("center","scale"),
  trControl = control,
  tuneGrid = expand.grid(
    my_models[["svm_tuning"]]$bestTune
  )
)

my_models[["svm"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[["svm"]]$evaluation <- model_evaluate(
  model = my_models[["svm"]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

################################## 



