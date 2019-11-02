# Adding the ROC curve plot

# Retrieving AUC values...
lr_auc <- plyr::round_any(my_models[["glm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
svm_auc <- plyr::round_any(my_models[["svm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
rf_auc <- plyr::round_any(my_models[["rf"]]$evaluation$roc$auc, accuracy=.001, f=floor)

# Random Forest ROC
ggroc(
  list(
    "Logistic Regression" = my_models[["glm"]]$evaluation$roc,
    "Support Vector Machine" = my_models[["svm"]]$evaluation$roc,
    "Random Forest"= my_models[["rf"]]$evaluation$roc
  ),
    aes= c("linetype", "colour"),
    size = 0.7
  ) +
  theme_bw() +
  scale_colour_manual(
    values = config$theme$plots[3:1],
    labels = c(
      paste("Logistic Regression \n", "(AUC=", lr_auc, ")", sep=""),
      paste("Support Vector Machines \n", "(AUC=", svm_auc, ")", sep=""), 
      paste("Random Forest \n", "(AUC=", rf_auc, ")", sep="")
    )
  ) +
  scale_linetype_manual(
    values = c(1,5,6),
    labels = c(
      paste("Logistic Regression \n", "(AUC=", lr_auc, ")", sep=""),
      paste("Support Vector Machines \n", "(AUC=", svm_auc, ")", sep=""), 
      paste("Random Forest \n", "(AUC=", rf_auc, ")", sep="")
    )
  ) +
  xlab("Specificity") +
  ylab("Sensitivity") +
  geom_segment(
    aes(x = 1, xend = 0, y = 0, yend = 1), 
    color = "grey", 
    linetype = "dashed"
  ) +
  labs(
    colour = "",
    linetype = ""
  ) +
  ggtitle("ROC Curve") +
  theme(
    axis.ticks.y = element_blank(),
    title = element_text(size = 16),
    axis.title.x = element_text(size = 14, vjust = 1),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(
      size = 12,
      margin = ggplot2::margin(10, 0, 0, 0, unit = "pt")
    )
  )


#########################################
# Cleaning global environment
rm(lr_auc)
rm(svm_auc)
rm(rf_auc)
