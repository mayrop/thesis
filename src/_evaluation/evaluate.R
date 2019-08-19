my_resamples <- list(
  `Logistic Regression` = my_models[["glm"]],
  `SVM` = my_models[["svm"]],
  `Random Forests` = my_models[["rf"]]
)

my_metrics <- list(
  "glm" <- c(name = "Logistic Regression", my_models[["glm"]]$evaluation$metrics),
  "svm" <- c(name = "SVM", my_models[["svm"]]$evaluation$metrics),
  "rf" <- c(name = "Random Forests", my_models[["rf"]]$evaluation$metrics)
)

stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)


# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).
bwplot(resamps, layout=c(3, 1), main="Metric comparison between different methods (CV train set)")
bwplot(resamps, metric = "AUC", main="Confidence Intervals for the different methods (Specificity)")
dotplot(resamps, metric = "AUC", main="Confidence Intervals for the different methods (Specificity)")
dotplot(resamps, metric = "Sensitivity", main="Confidence Intervals for the different methods (Sens)")
dotplot(resamps, metric = "Specificity", main="Confidence Intervals for the different methods (Specificity)")

difValues <- diff(resamps, metric="AUC")
summary(difValues)
bwplot(difValues)
dotplot(difValues)

difValues <- diff(resamps, metric="Sensitivity")
summary(difValues)
bwplot(difValues)
dotplot(difValues)

difValues <- diff(resamps, metric="Specificity")
summary(difValues)
bwplot(difValues)
dotplot(difValues)


dotplot(resamps, metric = "AUC")

densityplot(resamps, pch="|", metric="AUC", col=config$theme$plots, lty=1:3)
densityplot(resamps, pch="|", metric="Sens", col=config$theme$plots, lty=1:3)
densityplot(resamps, pch="|", metric="Spec", col=config$theme$plots, lty=1:3)

densityplot(my_models[["glm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["svm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["rf"]], pch = "|", metric= "AUC", col=config$theme$plots)

difValues <- diff(resamps, metric="Specificity")
summary(difValues)
dotplot(difValues)

difValues <- diff(resamps, metric="Sensitivity")
summary(difValues)
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



