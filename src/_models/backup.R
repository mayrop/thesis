


stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)



title()
splom(resamps)

#plot(density(models[["rf"]]$))
#lines(density(MyData$Column2))
#densityplot(models[["rf"]], pch = "|")

minfo <- my_models[["glmnet"]]$modelInfo$parameters
HLTest(my_models[['glm']], g=6)

difValues <- diff(resamps, metric="ROC", adjustment="none")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)
dotplot(resamps, metric = "ROC")

difValues <- diff(resamps, metric="Spec", adjustment="none")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)

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



