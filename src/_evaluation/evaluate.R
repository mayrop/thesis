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


