library(data.table)

my_resamples <- list(
  `Logistic Regression` = my_models[["glm"]],
  `SVM Linear` = my_models[["svmLinear"]],
  `SVM Poly` = my_models[["svmPoly"]],
  `SVM Gaussian` = my_models[["svmRadial"]],
  `Random Forest` = my_models[["rf"]]
)

my_metrics <- list(
  "glm" <- c(name = "Logistic Regression", my_models[["glm"]]$evaluation$metrics),
  "svmLinear" <- c(name = "SVM Linear", my_models[["svmLinear"]]$evaluation$metrics),
  "svmPoly" <- c(name = "SVM Poly", my_models[["svmPoly"]]$evaluation$metrics),
  "svmRadial" <- c(name = "SVM Gaussian", my_models[["svmRadial"]]$evaluation$metrics),
  "rf" <- c(name = "Random Forest", my_models[["rf"]]$evaluation$metrics)
)

stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)

# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).
bwplot(resamps, layout=c(3, 1), main="Metric comparison between different methods (CV train set)")
dotplot(resamps, metric = "ROC", main="Confidence Intervals for the different methods (Specificity) (CV train set)")
dotplot(resamps, metric = "Sens", main="Confidence Intervals for the different methods (Sens) (CV train set)")
dotplot(resamps, metric = "Spec", main="Confidence Intervals for the different methods (Specificity) (CV train set)")

difValues <- diff(resamps, metric="ROC")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)
dotplot(resamps, metric = "ROC")

difValues <- diff(resamps, metric="Spec", adjustment="none")
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)

# http://www.kimberlycoffey.com/blog/2016/7/16/compare-multiple-caret-run-machine-learning-models
# http://www.kimberlycoffey.com/blog/2016/3/19/random-forest
# https://www.r-bloggers.com/using-rpart-to-figure-out-who-voted-for-trump/

importance(my_models$rf$finalModel)

plot(my_models$rf0, metric = "Sens")
#monotonic
#SAS Certification Prep Guide: Statistical Business Analysis Using SAS9
plot.data <- as.data.frame(plot(my_models$rf$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees")
rf.plot
remove(rf.plot, plot.data)