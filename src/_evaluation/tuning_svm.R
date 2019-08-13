# Creating plot for RF hyperparameters cost

# https://github.com/topepo/caret/blob/master/pkg/caret/R/plot.train.R#L165
costs <- as.character(unique(my_models[["svm_tuning"]]$results$C))

my_x <- my_models[["svm_tuning"]]$bestTune$sigma
my_y <- max(my_models[["svm_tuning"]]$results$AUC)

plot(
  my_models[["svm_tuning"]], 
  col = config$theme$plots,
  lty = c(1:length(costs)),
  plotType = "scatter",
  pch = 19,
  cex = 0.35,
  #ylab = list(label="AUC (Cross-Validation)"),
  key = list(
    corner = c(1, 0), 
    x = 0.95, 
    y = 0.05,
    background = "white",
    border = "darkgray",
    title = "C",
    cex.title = 1.1,
    lines = list(
      pch = 19,
      col = config$theme$plots, lty=c(1:length(costs))
    ),
    text = list(costs)
  )
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
        labels = "ROC is at its maximum when C=1"
      )
    }
  )
)

#########################################
# Cleaning global environment
rm(costs)
rm(my_x)
rm(my_y)
