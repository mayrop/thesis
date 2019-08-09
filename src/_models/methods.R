set.seed(config$seed)

control <- trainControl(
  method="cv", 
  number=config$n_resampling, 
  classProbs=TRUE, 
  summaryFunction=twoClassSummary, 
  allowParallel=FALSE
)

my_formula <- form.build(
  y="response_factor",
  x=predictors,
  regex=paste(config$predictors$valid_suffixes, collapse="|"),
  transformations=config$predictors$transformations
)

my_models <- list()
my_resamples <- list()
my_metrics <- list()
my_methods <- list(
  "glm" = list(
    name="Logistic Regression",
    preProc=c()    
  ),
  #"glm_ltr" = list(
  #  name="LR Backward Elimination (LTR)",
  #  form=formula(
  #    step(glm(my_formula, data=train.data, family=binomial()), test="LRT", trace=0)
  #  ),    
  #  method="glm",
  #  preProc=c()
  #),
  #"glm_aic" = list(
  #  method="glmStepAIC",
  #  name="LR Backward Elimination (AIC)"
  #),
  #"glm_bic" = list(
  #  method="glmStepAIC",
  #  name="LR Backward Elimination (BIC)"
  #),
  # https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
  #"glmnet" = list(
  #  name="Penalized Logistic Regression",
  #  tuneGrid=expand.grid(
  #    # For Penalized logistic regression: # alpha = 1 -> lasso, 0 -> ridge
  #    alpha=seq(0.05, 0.95, by=0.05),
  #    lambda=seq(0.001, 0.01, by=0.001)
  #  ), 
  #  optimizeGrid="glmnet",
  #  tunes=9
  #),
  "rf" = list(
    name="Random Forest",
    tuneGrid=expand.grid(.mtry=c(1:10)),
    params=list(
      ntree=100,
      importance=TRUE
    ),
    optimizeGrid="rf",
    allowParallel=TRUE
  ),
  "svmLinear"=list(
    name="SVM (svmLinear)",
    tuneGrid=expand.grid(
      C=c(.25, .5, 1, 10, 100)
    ),
    optimizeGrid="svm"
  ),
  "svmPoly"=list(
    name="SVM (svmPoly)",
    params=list(
      tuneLength=4
    ),
    tuneGrid=expand.grid(
      C=c(.01, .1, 1, 10, 100),
      degree=c(1, 5, 10, 20),
      scale=1
    ),
    optimizeGrid="svm",
    tunes=38
  ),
  "svmRadial"=list(
    name="SVM (svmRadial)",
    tuneGrid=expand.grid(
      C=c(.25, .5, 1),
      sigma=sort(c(2^c(-15,-10, -5, 0), 0.05))
    ),
    optimizeGrid="svm",
    tunes=5
  )
)
