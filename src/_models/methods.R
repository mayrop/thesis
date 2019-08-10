set.seed(config$seed)

control <- trainControl(
  method="cv", 
  number=config$n_resampling, 
  classProbs=TRUE, 
  summaryFunction=twoClassSummary, 
  allowParallel=FALSE,
  sampling = "up"
)

my_formula <- form.build(
  y="response_factor",
  x=predictors,
  regex=paste(config$predictors$valid_suffixes, collapse="|")
#  transformations=config$predictors$transformations
)

my_models <- list()
my_resamples <- list()
my_metrics <- list()
my_methods <- list(
  "glm" = list(
    name="Logistic Regression",
    family="binomial",
    preProc=c()    
  ),
  "rf" = list(
    name="Random Forest",
    family="binomial",
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
    family="binomial",
    tuneGrid=expand.grid(
      C=c(.25, .5, 1, 10, 100)
    ),
    optimizeGrid="svm"
  ),
  "svmPoly"=list(
    name="SVM (svmPoly)",
    family="binomial",
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
    family="binomial",
    tuneGrid=expand.grid(
      C=c(.25, .5, 1),
      sigma=sort(c(2^c(-15,-10, -5, 0), 0.05))
    ),
    optimizeGrid="svm",
    tunes=5
  )
)
