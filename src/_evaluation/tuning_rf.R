# Creating plot for RF hyperparameters cost

my_x <- my_models[["rf_tuning"]]$bestTune$mtry
my_y <- max(my_models[["rf_tuning"]]$results$AUC)

plot(
  my_models[["rf_tuning"]], 
  col = config$theme$plots[1],
  plotType = "scatter",
  pch = 19,
  cex = 0.35,
  xlab = list(label="mtry (# Randomly Selected Predictors)"),
  main = "Hyperparameter tuning"
) + as.layer(
  xyplot(
    y ~ x, 
    data = as.data.frame(cbind(x = my_x, y = my_y)),
    type = c("o"),
    cex = 3,
    col = "black",
    panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      ltext(
        x = x, y = y, pos = 4, offset = 2, cex = 0.8,
        labels = paste("AUC maximised")
      )
    }
  )
)

#########################################
# Cleaning global environment
rm(my_x)
rm(my_y)
