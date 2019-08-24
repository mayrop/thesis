my_resamples <- list(
  `Logistic Regression` = my_models[["glm_"]],
  `SVM` = my_models[["svm_"]],
  `Random Forests` = my_models[["rf"]]
)

my_metrics <- list(
  "glm" <- c(name = "Logistic Regression", my_models[["glm_"]]$evaluation$metrics),
  "svm" <- c(name = "SVM", my_models[["svm_"]]$evaluation$metrics),
  "rf" <- c(name = "RF", my_models[["rf"]]$evaluation$metrics),
  "glm_up" <- c(name = "Logistic Regression (Oversampling)", my_models[["glm_up"]]$evaluation$metrics),
  "svm_up" <- c(name = "SVM Oversampling", my_models[["svm_up"]]$evaluation$metrics),
  "rf_up" <- c(name = "RF Oversampling", my_models[["rf_up"]]$evaluation$metrics),
  "glm_down" <- c(name = "Logistic Regression Undersampling", my_models[["glm_down"]]$evaluation$metrics),
  "svm_down" <- c(name = "SVM Undersampling", my_models[["svm_down"]]$evaluation$metrics),
  "rf_down" <- c(name = "RF Undersampling", my_models[["rf_down"]]$evaluation$metrics)
  #"rf" <- c(name = "Random Forests", my_models[["rf"]]$evaluation$metrics)
)

predictions <- cbind(
  test.data$response_factor,
  my_models[["rf"]]$evaluation$raw,
  my_models[["rf_up"]]$evaluation$raw
)

colnames(predictions) <- c("good", "none", "oversampling")

# both correct
a0 <- sum(predictions[, 1] == predictions[, 2] & predictions[, 1] == predictions[, 3])

# no stratification correct
a1 <- sum(predictions[, 1] == predictions[, 2] & predictions[, 1] != predictions[, 3])

# oversampling correct
a2 <- sum(predictions[, 1] != predictions[, 2] & predictions[, 1] == predictions[, 3])

# both incorrect
a3 <- sum(predictions[, 1] != predictions[, 2] & predictions[, 1] != predictions[, 3])

stats <- rbindlist(my_metrics)

stats %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  select(name, Accuracy, Sensitivity, Specificity, PosPredValue, NegPredValue, AUC) %>%
  kable(
    caption = 'Evaluation metrics.',
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position") %>%
  pack_rows("Group 1", 1, 3)




response <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

predictions <- cbind(
  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
)

predictions <- as.data.frame(predictions)

colnames(predictions) <- 1:ncol(predictions)

# calculate G
G <- rep(0, length.out = ncol(predictions))

for (i in 1:ncol(predictions)) {
  G[i] <- sum(response == predictions[, i])
}

predictions$correct <- response

correct <- apply(predictions[,], 1, function(row) {
  count_if(row[length(row)], row[-length(row)])
})

C <- ncol(predictions) - 1
Q <- (C-1) * (C * sum(G**2) - sum(G)**2) / (C*sum(G) - sum(q$correct**2))



resamps <- resamples(my_resamples)

Performance <- matrix(c(a0, a1, a2, a3),
         nrow = 2,
         dimnames = list("Oversampling" = c("Correct", "Incorrect"),
                         "No sampling" = c("Correct", "Incorrect")))
Performance
mcnemar.test(Performance)

# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).
bwplot(resamps, layout=c(3, 1), main="Metric comparison between different methods (CV train set)")
bwplot(resamps, metric = "AUC", main="Confidence Intervals for the different methods (Specificity)")
dotplot(resamps, metric = "AUC", main="Confidence Intervals for the different methods (Specificity)")
dotplot(resamps, metric = "Sensitivity", main="Confidence Intervals for the different methods (Sens)")
dotplot(resamps, metric = "Specificity", main="Confidence Intervals for the different methods (Specificity)")

difValues <- diff(resamps)
summary(difValues)
bwplot(difValues, layout=c(3, 1))
dotplot(difValues)

difValues <- diff(resamps, metric="AUC")
summary(difValues)
bwplot(difValues)
dotplot(difValues)

Sens <- diff(resamps, metric="Sensitivity")
summary(difValues)
bwplot(Sens)
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

ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + 
  geom_line(colour="#000099") + 
  xlab("Number of Decision Trees") + 
  ylab("Mean Squared Error") + 
  ggtitle("Out-of-bag (OOB) Error")
rf.plot
remove(rf.plot, plot.data)



