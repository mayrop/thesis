# Goodness of fit test
library(LOGIT)
HLTest(my_models[["glm"]]$finalModel, g = 8)
HLTest(my_models[["glm_up"]]$finalModel, g = 8)

my_resamples <- list(
  `Logistic Regression` = my_models[["glm"]],
  `SVM` = my_models[["svm_tuning"]],
  `Random Forests` = my_models[["rf"]]
)

for (algorithm in c("glm", "svm", "rf")) {
  algorithm_up <- paste(algorithm, "_up", sep = "")
  
  my_results[["contigencies"]][[algorithm]] <- get_contigency_table(
    test.data$response_factor,
    model1 = my_models[[algorithm]]$evaluation$raw,
    model2 = my_models[[algorithm_up]]$evaluation$raw,
    names = c("Default", "Oversampling")
  )

  # train data
  my_results[["train"]][[algorithm]] <- c(
    name = config$algorithms[[algorithm]], 
    my_models[[algorithm]]$results
  )
  my_results[["train"]][[algorithm_up]] <- c(
    name = paste(config$algorithms[[algorithm]], " (Oversampling)"),
    my_models[[algorithm_up]]$results
  )
  
  # test data
  my_results[["test"]][[algorithm]] <- c(
    name = config$algorithms[[algorithm]], 
    my_models[[algorithm]]$evaluation$metrics
  )
  my_results[["test"]][[algorithm_up]] <- c(
    name = paste(config$algorithms[[algorithm]], " (Oversampling)"),
    my_models[[algorithm_up]]$evaluation$metrics
  )
  
  rm(algorithm_up)
}

# printing the metrics for train data
rbindlist(my_results[["train"]], fill = TRUE) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  select(
    -parameter, - sigma, -C, -mtry
  ) %>%
  mutate(
    AccuracyText = paste(Accuracy, " (", AccuracySD, ")", sep=""),
    SensitivityText = paste(Sensitivity, " (", SensitivitySD, ")", sep=""),
    SpecificityText = paste(Specificity, " (", SpecificitySD, ")", sep=""),
    PPVText = paste(PPV, " (", PPVSD, ")", sep=""),
    NPVText = paste(NPV, " (", NPVSD, ")", sep=""),
    AUCText = paste(AUC, " (", AUCSD, ")", sep="")
  ) %>%
  select(
    name,
    AccuracyText,
    SensitivityText,
    SpecificityText,
    PPVText,
    NPVText,
    AUCText
  ) %>% kable(
    caption = 'Evaluation metrics.',
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position") %>%
  pack_rows("Logistic Regression", 1, 2) %>%
  pack_rows("SVM", 3, 4) %>%
  pack_rows("Random Forest", 5, 6)

# printing the metrics for train data
rbindlist(my_results[["train"]], fill = TRUE) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  select(
    -parameter, - sigma, -C, -mtry
  ) %>%
  select(
    name,
    Accuracy,
    Sensitivity,
    Specificity,
    PPV,
    NPV,
    AUC
  ) %>% kable(
    caption = 'Evaluation metrics.',
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position")

rbindlist(my_results[["test"]], fill = TRUE) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  select(
    name,
    Accuracy,
    Sensitivity,
    Specificity,
    PosPredValue,
    NegPredValue,
    AUC
  ) %>% kable(
    caption = 'Evaluation metrics.',
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position")

# printing the metrics for test data
rbindlist(my_metrics[["test"]]) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  )





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
Q2 <- (C-1) * (C * sum((G - mean(G))**2)) / (C*sum(G) - sum(q$correct**2))


my_results[["contigencies"]]$glm



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



