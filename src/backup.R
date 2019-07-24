# votes

mapping_columns <- list(
  "pop_14" = "log2(pop_14)",
  "pop_density_10" = "log2(pop_density_10)"
)

formula <- ""

for (predictor in predictors) {
  if (!grepl("_13", predictor) & !grepl("_14", predictor)) {
    next
  }
  
  if (!(predictor %in% names(mapping_columns))) {
    mapping_columns[[predictor]] <- predictor
  }
  
  sep <- ifelse(formula == "", " ", " + ")
  formula <- paste(formula, mapping_columns[[predictor]], sep=sep)
}

formula <- as.formula(paste("party_republican_won_factor ~ ", formula, sep=""))

model = train(
  form = formula,
  data = train.data,
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

varImp(model)

summary(model)

models = list()

add_model <- function(model, name) {
  # hash <- digest(toString(summary(model)), "md5", serialize = FALSE)
  
  models[[name]]$model <<- model
  models[[name]]$i <<- length(models)
  models[[name]]$name <<- name
}

add_model(model, "testing4")

pred <- predict(model, newdata=test.data)
accuracy <- table(pred, test.data$party_republican_won_factor)

sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data=pred, as.factor(
  test.data$party_republican_won_factor)
)

prob <- predict(model, newdata=test.data, type="response")
prediction <- prediction(prob, test.data$party_republican_won_factor)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

predict(model, newdata=test.data, type="prob")

Null deviance: 1626.33  on 1867  degrees of freedom
Residual deviance:  668.18  on 1858  degrees of freedom
AIC: 688.18

Number of Fisher Scoring iterations: 7

> model$results
parameter       ROC      Sens     Spec      ROCSD     SensSD     SpecSD
1      none 0.9532191 0.6796552 0.972676 0.02050938 0.09506711 0.01200

Null deviance: 1626.33  on 1867  degrees of freedom
Residual deviance:  663.19  on 1852  degrees of freedom
AIC: 695.19

Number of Fisher Scoring iterations: 7

parameter       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
1      none 0.9538329 0.6836782 0.9714464 0.01720152 0.08185938 0.01554758

all_p_values <- coef(summary(model))[,4]
p_values <- all_p_values[all_p_values < 0.05]

formula <- ""

for (i in 1:length(names(p_values))) {
  predictor <- names(p_values)[i]
  if (predictor == "(Intercept)") {
    next
  }
  
  sep <- ifelse(formula=="", " ", " + ")
  formula <- paste(formula, predictor, sep=sep)
}

formula <- paste("party_republican_won_factor ~ ", formula, sep="")
formula <- (as.formula(formula))

confusionMatrix(data = test.data$pred, reference = test.data$obs)


accuracy(list(model), plotit=TRUE, digits=3)

evaluate <- evaluate_model(model, test.data, "party_republican_won_factor")

RMSPE(y_pred = evaluate$y_predictions + 1, y_true = evaluate$y_true + 1)

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

y <- train$party_won_num

# alpha = 1 -> lasso, 0 -> ridge
cv.elasticnet <- cv.glmnet(x, y, alpha = 0.5, family = "binomial",  type.measure = "deviance")

plot(cv.elasticnet)
coef(cv.elasticnet, s = "lambda.min")




predict(cv.elasticnet, newx=newx, s = "lambda.min", type = "class")



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
