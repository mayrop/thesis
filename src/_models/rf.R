################################## 
# R a n d o m . F o r e s t

print("Running Random Forest...")

control <- base_control
control$seeds <- get_seeds(seed=config$seed)
start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["rf"]] <- train(
  form = my_formula,
  data = train.data,
  method = "rf",  
  family = "binomial",  
  metric = "ROC",
  trControl = control,
  tuneGrid = expand.grid(
    .mtry = c(1:10)
  ),
  ntree=100,
  importance=TRUE
)

my_models[["rf"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[["rf"]]$evaluation <- model_evaluate(
  model = my_models[["rf"]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

################################## 



