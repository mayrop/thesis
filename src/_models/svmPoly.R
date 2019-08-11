################################## 
# S V M . P o l y n o m i a l . K e r n e l

print("Running SVM Polynomial Kernel...")

control <- base_control
control$seeds <- get_seeds(seed = config$seed, n_tunes = 38)
start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["svmPoly"]] <- train(
  form = my_formula,
  data = train.data,
  method = "svmPoly",  
  family = "binomial",
  metric = "ROC",
  preProcess = c("center","scale"),
  tuneLength = 4,
  trControl = control,
  tuneGrid = expand.grid(
    C = c(.01, .1, 1, 10, 100),
    degree = c(1, 5, 10),
    scale = 1
  )
)

my_models[["svmPoly"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[["svmPoly"]]$evaluation <- model_evaluate(
  model = my_models[["svmPoly"]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

################################## 



