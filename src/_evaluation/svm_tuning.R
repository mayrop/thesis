# Creating plot for SVM hyperparameters cost
my_x <- my_models[["svm_tuning"]]$bestTune$sigma
my_y <- max(my_models[["svm_tuning"]]$results$AUC)

my_lines <- my_models[["svm_tuning"]]$results[, c("C", "sigma", "AUC")] %>% 
  spread("C", "AUC")

# For removing margin from axis
par(mgp = c(1.8, 0.6, 0))

matplot(
  my_lines[,1], my_lines[,-1], 
  type = "l", 
  col = config$theme$plots, 
  lty = c(3, 2, 1, 5), 
  lwd = 2,
  xlab = expression(sigma), 
  ylab = "AUC (Cross-Validation)",
  panel.first = grid(),
  mar = c(0, 0, 0, 0)
)

points(
  my_x,
  my_y,
  type = c("o"),
  cex = 3,
  col = config$theme$plots[1]
)
title("SVM Hyperparameters Tuning", line = 0.7)

legend(
  "bottomright", 
  lty = c(3, 2, 1, 5), 
  lwd = 2, 
  col = config$theme$plots, 
  colnames(my_lines)[-1],
  title = "C"
)

#########################################
# Cleaning global environment
rm(my_lines)
rm(my_x)
rm(my_y)
