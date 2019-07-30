# votes
library(LOGIT)
library(data.table)
library(doParallel)

install.packages("https://cran.r-project.org/src/contrib/Archive/LOGIT/LOGIT_1.3.tar.gz", repos=NULL, type="source")

get_tune_grid <- function(model, type) {
  # coef(models[["glmnet"]]$finalModel, models[["glmnet"]]$finalModel$lambdaOpt)
  if (type == "glmnet") {
    return(expand.grid(
      as.list(model$finalModel$tuneValue)
    ))
  }
  
  if (type == "rf") {
    return(expand.grid(
      as.list(model$bestTune)
    ))
  }
}

get_my_model <- function(method, data, control, family="binomial", metric="ROC", preProc=c("center", "scale")) {
  # ifelse is vectorized, so use `if`
  my_list = list(
    form=data$form,
    data=data$data,
    method=`if`(!is.null(data$method), data$method, method),  
    family=`if`(!is.null(data$family), data$family, family),
    metric=`if`(!is.null(data$metric), data$metric, metric),
    trControl=`if`(!is.null(data$control), data$control, control),
    preProc=`if`(is.array(data$preProc), data$preProc, preProc),
    tuneGrid=`if`(!is.null(data$tuneGrid), data$tuneGrid, c())
  )
  
  params=`if`(is.list(data$params), data$params, list())  
  
  set.seed(config$seed)
  
  # multithreading....
  cl <- makeCluster(3)
  registerDoParallel(cl)

  # https://www.r-bloggers.com/a-new-r-trick-for-me-at-least/
  my_model <- do.call('train', as.list(c(my_list, params)))
  
  stopCluster(cl)
  registerDoSEQ()
  rm(cl)
  
  return(my_model)
}


# length is = (nresampling)+1
seeds <- vector(mode = "list", length = nrow(train.data) + 1)
seeds <- lapply(seeds, function(x) {
  1:20
})
seeds[[nrow(train.data) + 1]] <- 1 # 1 for the last one

control <- trainControl(
  method="cv", 
  number=10, 
  classProbs=TRUE, 
  summaryFunction=twoClassSummary, 
  seeds=seeds,
  allowParallel=FALSE
)

sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data=pred, as.factor(
  test.data$party_republican_won_factor)
my_formula <- build_initial_formula(
  response="response_factor",
  predictors=predictors,
  regex=paste(config$predictors$valid_suffixes, collapse="|"),
  transformations=config$predictors$transformations
)

prob <- predict(model, newdata=test.data, type="response")
prediction <- prediction(prob, test.data$party_republican_won_factor)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
my_models <- list()
my_methods <- list()
my_resamples <- list()
my_metrics <- list()

predict(model, newdata=test.data, type="prob")

my_methods[["glm"]] <- list(
  name="Logistic Regression",
  form=formula(
    step(glm(my_formula, data=train.data, family=binomial()), test="LRT", trace=0)
  ),
  preProc=c()
)
my_methods[["glm_ltr"]] <- list(
  name="LR Backward Elimination (LTR)",
  method="glm",
  form=formula(
    step(glm(my_formula, data=train.data, family=binomial()), test="LRT", trace=0)
  ),
  preProc=c()
)
my_methods[["glmStepAIC"]] <- list(
  name="LR Backward Elimination (AIC)"
)
my_methods[["glmStepBIC"]] <- list(
  name="LR Backward Elimination (BIC)",
  method="glmStepAIC",
  params=list(
    k=log(nrow(train.data)) 
  )
)
my_methods[["glmnet"]] <- list(
  name="Penalized Logistic Regression",
  tuneGrid=expand.grid(
    # alpha = 1 -> lasso, 0 -> ridge
    alpha=seq(0.05, 0.95, by=0.05),
    lambda=seq(0.001, 0.01, by=0.001)
  ), 
  optimizeGrid="glmnet"
)
my_methods[["rf"]] <- list(
  name="Random Forest",
  tuneGrid=expand.grid(.mtry=c(1:10)),
  params=list(
    ntree=100,
    importance=TRUE
  ),
  optimizeGrid="rf",
  allowParallel=TRUE
)
my_methods[["svmLinear"]] <- list(
  name="SVM with Linear Kernel",
  params=list(
  )
)
my_methods[["svmPoly"]] <- list(
  name="SVM with Polynomial Kernel",
  params=list(
  )
)
my_methods[["svmRadial"]] <- list(
  name="Radial Basis Function Kernel",
  params=list(
  )
)



for (method in names(my_methods)) {
  ################################## 
  control$allowParallel <- `if`(!is.null(my_methods[[method]]$allowParallel), my_methods[[method]]$allowParallel, FALSE)
  
  print(paste("Running", method))
  start_time <- Sys.time()
  
  # defaulting the formula
  my_methods[[method]]$form = `if`(!is.null(my_methods[[method]]$form), my_methods[[method]]$form, my_formula)
  my_methods[[method]]$data = `if`(!is.null(my_methods[[method]]$data), my_methods[[method]]$data, train.data)
  
  my_models[[method]] = get_my_model(method, my_methods[[method]], control=control)
  
  my_models[[method]]$benchmarks = list(
    start=start_time,
    end=Sys.time()
  )
  
  # Get the optimized parameters
  if (!is.null(my_methods[[method]]$optimizeGrid)) {
    start_time <- Sys.time()
    
    my_methods[[method]]$tuneGrid = get_tune_grid(my_models[[method]], my_methods[[method]]$optimizeGrid)
    print("New optimized grid...")
    print(my_methods[[method]]$tuneGrid)
    my_methods[[method]]$optimizeGrid = NULL
    
    print("Running again...")
    my_models[[method]] = get_my_model(method, my_methods[[method]], control=control)
    
    my_models[[method]]$benchmarks$tuning <- list(
      start=start_time,
      end=Sys.time()
    )
  }
  
  ##################################
  # Getting the values for the test data
  
  post <- list()
  
  post$probs <- predict(my_models[[method]], newdata=test.data,  type="prob")
  post$raw <- predict(my_models[[method]], newdata=test.data,  type="raw")
  post$roc <- roc(
    response = test.data$response_factor,
    predictor = post$probs[, "yes"],
    levels = levels(test.data$response_factor)
  )
  post$matrix <- confusionMatrix(
    data=post$raw, test.data$response_factor
  )

accuracy(list(model), plotit=TRUE, digits=3)
  post$metrics <- c(
    post$matrix$byClass, 
    as.list(post$matrix$overall), 
    list(AUC=post$roc$auc)
  )
  
  # Removing spaces
  names(post$metrics) <- sapply(names(post$metrics), function(name) {
    return(gsub(" ", "", name))
  })
  
  my_models[[method]]$post <- post
  
  ##################
  my_name <- my_methods[[method]]$name
  my_resamples[[my_name]] <- my_models[[method]]
  
  ################
  my_metrics[[method]] <- c(
    name=my_name,
    post$metrics
  )
}

evaluate <- evaluate_model(model, test.data, "party_republican_won_factor")

RMSPE(y_pred = evaluate$y_predictions + 1, y_true = evaluate$y_true + 1)
stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)

plot(evaluate$performance)



evaluate_model <- function(model, data, response_column, prob=0.5) {
  probs <- predict(model, data, type="response")
  predictions <- ifelse(probs > prob, 1, 0)
  
  index_response <- which(colnames(data)==response_column)
  
  p <- predict(model, data, type="response")
  pr <- prediction(p, data[index_response])
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  return(list(
    performance=prf,
    auc=auc,
    y_predictions=as.vector(predictions),
    y_true=data[[index_response]],
    mean_predictions=mean(predictions == data[index_response]),
    table_predictions=table(predictions, data[[index_response]])
  ))
}

##### Penalized logistic regression

x <- model.matrix(formula, train)
newx <- model.matrix(formula, test)
bwplot(resamps, layout = c(3, 1))
dotplot(resamps, metric = "ROC")
splom(resamps)

y <- train$party_won_num

# alpha = 1 -> lasso, 0 -> ridge
cv.elasticnet <- cv.glmnet(x, y, alpha = 0.5, family = "binomial",  type.measure = "deviance")

plot(cv.elasticnet)
coef(cv.elasticnet, s = "lambda.min")




predict(cv.elasticnet, newx=newx, s = "lambda.min", type = "class")
difValues <- diff(resamps, metric="ROC", adjustment="none")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)
dotplot(resamps, metric = "ROC")



HLTest(model.ridge, g=6)

pR2(glm.fit)


evaluation <- evaluate_model(model, test, "party_won")


Anova(model, 
      type="II", 
      test="Wald")

nagelkerke(model)
summary(model)

emplogit(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)
empLogitPlot(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)

ggplot(data = melted_correlation, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




data(Default, package = "ISLR")
library(caret)

default_glm_mod = train(
  form = default ~ student + balance,
  data = default_trn,
  trControl = trainControl(
    method = "cv", 
    number = 10,
    classProbs=TRUE, 
    summaryFunction=twoClassSummary
  ),
  method = "glm",
  family = "binomial",
  metric="ROC"
)


default_glm_mod2 = train(
  form = default ~ student + balance,
  data = default_trn,
  trControl = trainControl(
    method = "cv", 
    number = 10
  ),
  method = "glm",
  family = "binomial"
)

# https://statisticalhorizons.com/wp-content/uploads/GOFForLogisticRegression-Paper.pdf
print(default_glm_mod)

names(default_glm_mod)

default_glm_mod$results

default_glm_mod$finalModel

summary(default_glm_mod)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

# test acc
calc_acc(actual = default_tst$default,
         predicted = predict(default_glm_mod, newdata = default_tst))

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}
source("http://bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")

biocLite("limma")
library(limma)

get_best_result(default_glm_mod)
library(MKmisc)
HLgof.test(fit = fitted(mod_fit_one), obs = training$Class)
