################################## 
# L o g i s t i c . R e g r e s s i o n

print("Running glm (logistic regression)...")

control <- trainControl(
  method = "cv",
  number = 10, 
  summaryFunction = function(...) { 
    # this is an overwrite of twoClassSummary from caret
    # this is to prevent sens & spec to be switched
    two_class_summary(...)
  }, 
  classProbs = TRUE, 
  seeds = get_seeds(seed = config$seed)
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

index <- paste("glm", sampling, sep = "_")
index <- gsub("_$", "", index)

my_models[[index]] <- train(
  form = my_formula,
  data = train.data,
  method = "glm",  
  family = "binomial",
  metric = "AUC",
  trControl = control
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

################################## 



