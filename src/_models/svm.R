################################## 
# S V M . R a d i a l . K e r n e l

print("Running SVM Radial Kernel...")

control <- trainControl(
  method = "cv",
  number = 10, 
  summaryFunction = function(...) { 
    # this is an overwrite of twoClassSummary from caret
    # this is to prevent sens & spec to be switched
    two_class_summary(...)
  }, 
  classProbs = TRUE, 
  seeds = get_seeds(seed = config$seed, n_tunes = 38)
)

if (sampling != "") {
  control$sampling <- sampling
}

start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

# Setting the formula...
my_formula <- as.formula(paste(
  "response_factor ~",
  paste(predictors, collapse = " + ")
))

index <- paste("svm_tuning_", sampling, sep = "")

my_models[[index]] <- train(
  form = my_formula,
  data = train.data,
  method = "svmRadial",  
  family = "binomial",
  metric = "AUC",
  preProcess = c("center", "scale"),
  trControl = control,
  tuneGrid = expand.grid(
    C = c(0.1, .5, 1, 10),
    sigma = sort(c(2^c(-15, -10, -5, -4, -3)))
  )
)

my_models[[index]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

# training with the best tune...
print("Now training with the best tune...")
start_time <- Sys.time()
# Resseting seed...
set.seed(config$seed)

best_tune <- my_models[[index]]$bestTune
index <- paste("svm_", sampling, sep = "")

my_models[[index]] <- train(
  form = my_formula,
  data = train.data,
  method = "svmRadial",  
  family = "binomial",
  metric = "AUC",
  preProcess = c("center","scale"),
  trControl = control,
  tuneGrid = expand.grid(
    best_tune
  )
)

my_models[[index]]$benchmarks <- list(
  start = start_time,
  end = Sys.time()
)

my_models[[index]]$evaluation <- model_evaluate(
  model = my_models[[index]], 
  data = test.data, 
  response = "response_factor",
  levels = my_levels
)

#########################################
# Cleaning global environment
rm(index)
rm(best_tune)

