


curve(predict(my_models[["rf"]]$finalModel,
              data.frame(Continuous=x),
              type="response"
            ), 
      lty=1, lwd=2, col="blue",                            
      add=TRUE)



stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)


grp10 <- HLTest(obj=models[["glmnet"]]$finalModel, g=6)
cbind(grp10$observed, round(grp10$expect, digits = 1))
grp10


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

