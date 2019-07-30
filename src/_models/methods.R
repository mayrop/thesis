
my_methods <- list(
  "glm" = list(
    name="Logistic Regression",
    preProc=c()    
  ),
  "glm_ltr" = list(
    name="LR Backward Elimination (LTR)",
    form=formula(
      step(glm(my_formula, data=train.data, family=binomial()), test="LRT", trace=0)
    ),    
    method="glm",
    preProc=c()
  ),
  "glm_aic" = list(
    method="glmStepAIC",
    name="LR Backward Elimination (AIC)"
  ),
  "glm_bic" = list(
    method="glmStepAIC",
    name="LR Backward Elimination (BIC)"
  ),
  "glmnet" = list(
    name="Penalized Logistic Regression",
    tuneGrid=expand.grid(
      alpha=seq(0.05, 0.95, by=0.05),
      lambda=seq(0.001, 0.01, by=0.001)
    ), 
    optimizeGrid="glmnet"
  ),
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
    name="SVM with Linear Kernel"
  ),
  "svmPoly"=list(
    name="SVM with Polynomial Kernel"
  ),
  "svmRadial"=list(
    name="Radial Basis Function Kernel"
  )
)