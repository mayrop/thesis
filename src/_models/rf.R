################################## 
# R a n d o m . F o r e s t

print("Running Random Forest...")

control <- trainControl(
  method = "cv",
  number = 10, 
  summaryFunction = function(...) { 
    # this is an overwrite of twoClassSummary from caret
    # this is to prevent sens & spec to be switched
    two_class_summary(...)
  }, 
  classProbs = TRUE, 
  seeds = get_seeds(seed=config$seed, n_tunes = length(predictors))
)

if (sampling != "") {
  control$sampling <- sampling
}

start_time <- Sys.time()

# Resseting seed...
set.seed(config$seed)

index <- paste("rf_tuning_", sampling, sep = "")
index <- gsub("_$", "", index)

#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-25
my_models[[index]] <- train(
  form = my_formula,
  data = train.data,
  method = "rf",  
  family = "binomial",  
  metric = "AUC",
  trControl = control,
  tuneGrid = expand.grid(
    .mtry = c(1:length(predictors))
  ),
  ntree = 100,
  importance=TRUE
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

best_tune = my_models[[index]]$bestTune
index <- paste("rf", sampling, sep = "_")
index <- gsub("_$", "", index)

my_models[[index]] <- train(
  form = my_formula,
  data = train.data,
  method = "rf",  
  family = "binomial",  
  metric = "AUC",
  trControl = control,
  tuneGrid = expand.grid(as.list(
    best_tune
  )),
  ntree = 100,
  importance = TRUE
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
rm(control)
rm(start_time)



