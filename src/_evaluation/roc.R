# Adding the ROC curve plot

# Retrieving AUC values...
lr_auc <- plyr::round_any(my_models[["glm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
svm_auc <- plyr::round_any(my_models[["svm"]]$evaluation$roc$auc, accuracy=.001, f=floor)
rf_auc <- plyr::round_any(my_models[["rf"]]$evaluation$roc$auc, accuracy=.001, f=floor)

# First the Logistic Regression Plot
plot(
  my_models[["glm"]]$evaluation$roc, 
  col = config$theme$plots[1], 
  lty = 1, 
  legacy.axes = 1, 
  main="ROC Curve"
)

# Then the SVM Plot
plot(
  my_models[["svm"]]$evaluation$roc, 
  col = config$theme$plots[2],  
  lty = 2, 
  legacy.axes = 1, 
  add = TRUE
)

# Finally, the Random Forest one
plot(
  my_models[["rf"]]$evaluation$roc, 
  col = config$theme$plots[3], 
  add = TRUE, 
  lty = 3, 
  legacy.axes = 1
)

# Adding a line on x and y axis..
abline(lwd=0.5, lty=2, v=1, h=1, col="gray")

# Adding legend
legend("right", 
  legend = c(
    paste("Logistic Regression: ", "(AUC=", lr_auc, ")", sep=""), 
    paste("Support Vector Machines: ", "(AUC=", svm_auc, ")", sep=""), 
    paste("Random Forest: ", "(AUC=", rf_auc, ")", sep="")
  ),
  col = config$theme$plots, 
  lty = c(1:3), 
  lwd = 2,
  cex = 0.8
)

#########################################
# Cleaning global environment
rm(lr_auc)
rm(svm_auc)
rm(rf_auc)
