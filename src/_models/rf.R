################################## 
# R a n d o m . F o r e s t

print("Running Random Forest...")

control <- base_control
control$seeds <- get_seeds(seed=config$seed, n_tunes=15)
start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["rf_tuning"]] <- train(
  form = my_formula,
  data = train.data,
  method = "rf",  
  family = "binomial",  
  metric = "AUC",
  trControl = control,
  tuneGrid = expand.grid(
    .mtry = c(1:15)
  ),
  ntree=100,
  importance=TRUE
)

# training with the best tune...
print("Now training with the best tune...")
my_models[["rf"]] <- train(
  form = my_formula,
  data = train.data,
  method = "rf",  
  family = "binomial",  
  metric = "AUC",
  trControl = control,
  tuneGrid = expand.grid(as.list(
    my_models[["rf_tuning"]]$bestTune
  )),
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



