################################## 
# S V M . L i n e a r . K e r n e l

print("Running SVM Linear Kernel...")

control <- base_control
control$seeds <- get_seeds(seed=config$seed)
start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["svmLinear"]] <- caret::train(
  form = my_formula,
  data = train.data,
  method = "svmLinear",  
  metric = "ROC",
  preProcess = c("center","scale"),
  trControl = control,
  tuneGrid = expand.grid(
    C = c(.25, .5, 1, 10, 100)
  )
)

my_models[["svmLinear"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[["svmLinear"]]$evaluation <- model_evaluate(
  model = my_models[["svmLinear"]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

################################## 



