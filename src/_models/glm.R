################################## 
# L o g i s t i c . R e g r e s s i o n

print("Running glm (logistic regression)...")

control <- base_control
control$seeds <- get_seeds(seed=config$seed)

start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

my_models[["glm"]] <- train(
  form = my_formula,
  data = train.data,
  method = "glm",  
  family = "binomial",
  metric = "AUC",
  trControl = control
)

my_models[["glm"]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[["glm"]]$evaluation <- model_evaluate(
  model = my_models[["glm"]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

################################## 



