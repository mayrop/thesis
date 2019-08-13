# Adding the ROC curve plot

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
  my_models[["svmRadial"]]$evaluation$roc, 
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
    "Logistic Regression", 
    "Support Vector Machines",
    "Random Forest"
  ),
  col = config$theme$plots, 
  lty = c(1:3), 
  lwd = 2,
  cex = 0.8
)
