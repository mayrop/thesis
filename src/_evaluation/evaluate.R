# Goodness of fit test
HLTest(my_models[["glm"]]$finalModel, g = 8)
HLTest(my_models[["glm_up"]]$finalModel, g = 8)

# Get the metrics for the 3 algorithms
for (algorithm in c("glm", "svm", "rf")) {
  algorithm_up <- paste(algorithm, "_up", sep = "")
  
  my_results[["mcnemar"]][[algorithm]] <- eval.mcnemar(
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

################################################## 
#
# Metrics for train data
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

################################################## 
#
# printing the metrics for test data
rbindlist(my_results[["test"]]) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  )


################################################## 
#
# McNemar test
my_results[["mcnemar"]]$glm
my_results[["mcnemar"]]$svm
my_results[["mcnemar"]]$rf

#response <- c(0, 0, 0, "a", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#predictions <- cbind(
#  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
#  c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#  c(1, 1, 1, "a", 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
#)

response <- as.numeric(test.data$response_factor)

predictions <- cbind(
  as.numeric(my_models[["svm"]]$evaluation$raw),
  as.numeric(my_models[["rf"]]$evaluation$raw)
)

predictions <- as.data.frame(predictions)

eval.cochrans_q(response, predictions)



# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).


my_resamples <- list(
  `Logistic Regression` = my_models[["glm"]],
  `SVM` = my_models[["svm_tuning"]],
  `Random Forests` = my_models[["rf"]]
)
resamps <- resamples(my_resamples)

dotplot(resamps, metric = "AUC")

densityplot(resamps, pch="|", metric="AUC", col=config$theme$plots, lty=1:3)

densityplot(my_models[["glm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["svm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["rf"]], pch = "|", metric= "AUC", col=config$theme$plots)

difValues <- diff(resamps, metric="AUC")
summary(difValues)
dotplot(difValues)

importance(my_models$rf$finalModel)
varImpPlot(my_models$rf$finalModel)

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
  ggtitle("Out-of-bag (OOB) Error") +
  theme_bw()
remove(plot.data)



