# Goodness of fit test
HLTest(my_models[["glm"]]$finalModel, g = 8)
HLTest(my_models[["glm_up"]]$finalModel, g = 8)

# Get the metrics for the 3 algorithms
for (algorithm in c("glm", "svm", "rf")) {
  # train data
  my_results[["train"]][[algorithm]] <- c(
    name = config$algorithms[[algorithm]], 
    my_models[[algorithm]]$results
  )

  # test data
  my_results[["test"]][[algorithm]] <- c(
    name = config$algorithms[[algorithm]], 
    my_models[[algorithm]]$evaluation$metrics
  )
}

################################################## 
#
# Metrics for all classifiers...
df1 <- rbindlist(my_results[["train"]], fill = TRUE) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  select(
    -parameter, -sigma, -C, -mtry
  ) %>%
  mutate(
    Set = "Train",
    AccuracyText = paste(Accuracy, " (", AccuracySD, ")", sep=""),
    SensitivityText = paste(Sensitivity, " (", SensitivitySD, ")", sep=""),
    SpecificityText = paste(Specificity, " (", SpecificitySD, ")", sep=""),
    PPVText = paste(PPV, " (", PPVSD, ")", sep=""),
    NPVText = paste(NPV, " (", NPVSD, ")", sep=""),
    AUCText = paste(AUC, " (", AUCSD, ")", sep="")
  ) %>%
  select(
    name,
    Set,
    AccuracyText,
    SensitivityText,
    SpecificityText,
    PPVText,
    NPVText,
    AUCText
  )

df2 <- rbindlist(my_results[["test"]], fill = TRUE) %>% 
  dplyr::mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  ) %>%
  mutate(
    Set = "Test",
    AccuracyText = as.character(Accuracy),
    SensitivityText = as.character(Sensitivity),
    SpecificityText = as.character(Specificity),
    PPVText = as.character(PosPredValue),
    NPVText = as.character(NegPredValue),
    AUCText = as.character(AUC)
  ) %>%
  select(
    name,
    Set,
    AccuracyText,
    SensitivityText,
    SpecificityText,
    PPVText,
    NPVText,
    AUCText
  ) 

as.data.frame(
    t(
      union(df1, df2) %>% 
      # Putting 2 cols into 1
        tidyr::unite("name", c(name, Set))
    )
  ) %>%
  dplyr::select(
    V1,
    V4,
    V2,
    V5,
    V3,
    V6
  )
#%>% kable(
#    caption = 'Evaluation metrics.',
#    booktabs = TRUE, 
#    format = "latex"
#  ) %>%
#  kable_styling(latex_options = "HOLD_position") %>%
#  add_header_above(c(" ", "Logistic Regression" = 2, "SVM" = 2, "Random Forest" = 2))




################################################## 
#
# McNemar's Test

#
response <- c(0, 0, 0, "a", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

predictions <- cbind(
  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, "a", 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
)

eval.cochrans_q(response, as.data.frame(predictions))
# Cochrans Q Test

my_differences <- list()

y <- as.numeric(test.data$response_factor)

for (i in 1:(length(config$algorithms) - 1)) {
  for (j in (i+1):length(config$algorithms)) {
    comb <- c(names(config$algorithms)[i], names(config$algorithms)[j])
    index <- paste(comb, collapse = "_")

    my_differences[[index]] = eval.mcnemar(
      y,
      as.numeric(my_models[[comb[1]]]$evaluation$raw),
      as.numeric(my_models[[comb[2]]]$evaluation$raw),
      names = c(
        config$algorithms[[i]],
        config$algorithms[[j]]
      )
    )
  }
}

my_data_frame <- data.frame(rbind(
  my_differences$glm_svm$classifications,
  my_differences$glm_rf$classifications,
  my_differences$svm_rf$classifications
))

my_data_frame$names <- c(
  "LR (A) - SVM (B)", 
  "LR (A) - RF (B)", 
  "SVM (A) - RF (B)"
)


my_data_frame <- my_data_frame %>% 
  gather("group", "value", -names)

ggplot(
    my_data_frame, aes(names, value, fill = group)
  ) + 
  geom_bar(stat = "identity", position = "dodge2") + 
  scale_fill_manual(
    values = rev(config$theme$plots),
    name = "Predictions", 
    labels = c(
      "Both models predicted correctly", 
      "Only model A predicted correctly", 
      "Only model B predicted correctly", 
      "Both models predicted incorrectly"
    )
  ) +
  geom_text(
    aes(
      label = value
    ), 
    vjust = -0.8,
    position = position_dodge2(
      width = 0.9,
      padding = 2
    )
  ) +
  xlab("Algorithms Comparison") +
  ylab("# predictions") +
  scale_y_continuous(trans = log10_trans()) +
  theme_bw()

# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).


my_resamples <- list(
  `Logistic Regression` = my_models[["glm"]],
  `SVM` = my_models[["svm_tuning"]],
  `Random Forests` = my_models[["rf"]]
)
resamps <- resamples(my_resamples)

dotplot(resamps, metric = "AUC")

densityplot(
  resamps, 
  pch="|", 
  metric="AUC", 
  col=config$theme$plots, lty=1:3
)

densityplot(my_models[["glm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["svm"]], pch = "|", metric= "AUC", col=config$theme$plots)
densityplot(my_models[["rf"]], pch = "|", metric= "AUC", col=config$theme$plots)

difValues <- diff(resamps, metric="AUC")
summary(difValues)
dotplot(difValues)

importance(my_models$rf$finalModel)
varImpPlot(my_models$rf$finalModel)

#plot(my_models$rf0, metric = "Sens")
#monotonic
#SAS Certification Prep Guide: Statistical Business Analysis Using SAS9

