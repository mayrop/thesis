# Creating plot for RF error

rf_data <- as.data.frame(plot(my_models$rf$finalModel))
colnames(rf_data) <- c("Error")
rf_data$trees <- as.numeric(rownames(rf_data))

plot(
  x = rf_data$trees, 
  y = rf_data$Error,
  type = "l",
  xlab = "Number of Decision Trees", 
  ylab = "Mean Squared Error",
  main = "Out-of-bag (OOB) Error",
  panel.first = grid()
)

#########################################
# Cleaning global environment
rm(rf_data)
