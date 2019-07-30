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

my_formula <- build_initial_formula(
  response="response_factor",
  predictors=predictors,
  regex=paste(config$predictors$valid_suffixes, collapse="|"),
  transformations=config$predictors$transformations
)

my_models <- list()
my_resamples <- list()
my_metrics <- list()
source("_models/methods.R")


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


stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)


grp10 <- HLTest(obj=models[["glmnet"]]$finalModel, g=6)
cbind(grp10$observed, round(grp10$expect, digits = 1))
grp10

# https://stackoverflow.com/questions/48079660/extract-the-coefficients-for-the-best-tuning-parameters-in-caret

#compare_models(models[["glmnet"]], models[["glm_step_ltr"]])
#compare_models(models)
#The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).


bwplot(resamps, layout = c(3, 1))
dotplot(resamps, metric = "ROC")
splom(resamps)


#plot(density(models[["rf"]]$))
#lines(density(MyData$Column2))
#densityplot(models[["rf"]], pch = "|")


minfo <- my_models[["glmnet"]]$modelInfo$parameters
HLTest(models[['glm']],g=6)



difValues <- diff(resamps, metric="ROC", adjustment="none")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)
dotplot(resamps, metric = "ROC")



HLTest(model.ridge, g=6)

pR2(glm.fit)


Anova(model, 
      type="II", 
      test="Wald")

nagelkerke(models[["glm_step_ltr"]]$finalModel)
summary(model)

emplogit(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)
empLogitPlot(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)

ggplot(data = melted_correlation, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






# https://statisticalhorizons.com/wp-content/uploads/GOFForLogisticRegression-Paper.pdf
print(default_glm_mod)

names(default_glm_mod)

default_glm_mod$results

default_glm_mod$finalModel

summary(default_glm_mod)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

source("http://bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")

biocLite("limma")
library(limma)

get_best_result(default_glm_mod)
library(MKmisc)
HLgof.test(fit = fitted(mod_fit_one), obs = training$Class)



Xy <- train.data[,which(colnames(train.data) %in% c(predictors, "response_binary"))]
Xy %<>% 
  rename(
    y=response_binary
  ) %>% 
  mutate(
    pop_14 = log(pop_14),
    age_o65_pct_14 = log(age_o65_pct_14)
  ) %>% 
  select(
    -response_regression
  ) %>%
  select(
    -y, y
  )
Xy <- as.data.frame(Xy)
Xy$y <- as.integer(Xy$y)

library(bestglm)
BIC <- bestglm(Xy, IC="BIC", family=binomial)
AIC <- bestglm(Xy, IC="AIC", family=binomial)
CV <- bestglm(Xy, IC="CV", t=100)

maxvar <- 7 
direction <- "backward"

set.seed(config$seed)

models[["stepLDA"]] <- train(
  form = formula,
  data = train.data,
  trControl = controls[["cv"]],
  method = "stepLDA",
  family = "binomial",
  preProc = c("center", "scale"),
  metric = "ROC",
  tuneGrid = data.frame(maxvar, direction)
)

set.seed(config$seed)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

