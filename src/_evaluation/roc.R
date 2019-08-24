# Adding the ROC curve plot

# Retrieving AUC values...
lr_auc <- plyr::round_any(my_models[["glm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
svm_auc <- plyr::round_any(my_models[["svm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
rf_auc <- plyr::round_any(my_models[["rf"]]$evaluation$roc$auc, accuracy=.001, f=floor)

# Random Forest ROC
plot(
  my_models[["rf"]]$evaluation$roc, 
  col = config$theme$plots[3], 
  lty = 1, 
  legacy.axes = 1,
  lwd = 1.2,
  panel.first = grid(),
  main="ROC Curve"
)

# SVM ROC
plot(
  my_models[["svm"]]$evaluation$roc, 
  col = config$theme$plots[2],  
  lty = 2, 
  legacy.axes = 1, 
  add = TRUE,
  lwd = 1.2
)


# Logistic Regression ROC
plot(
  my_models[["glm"]]$evaluation$roc, 
  col = config$theme$plots[1], 
  lty = 3, 
  legacy.axes = 1, 
  add = TRUE, 
  lwd = 1.2
)

# Adding a line on x and y axis..
abline(lwd=0.5, lty=2, v=1, h=1, col="gray")

# Adding legend
legend("bottomright", 
  legend = c(
    paste("Logistic Regression: ", "(AUC=", lr_auc, ")", sep=""), 
    paste("Support Vector Machines: ", "(AUC=", svm_auc, ")", sep=""), 
    paste("Random Forest: ", "(AUC=", rf_auc, ")", sep="")
  ),
  col = config$theme$plots, 
  lty = c(3:1), 
  lwd = 2,
  cex = 0.7
)

#########################################
# Cleaning global environment
rm(lr_auc)
rm(svm_auc)
rm(rf_auc)
