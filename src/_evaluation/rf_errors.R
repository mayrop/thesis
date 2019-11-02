# Creating plot for RF error

rf_data <- as.data.frame(plot(my_models$rf$finalModel))
colnames(rf_data) <- c("Error")
rf_data$trees <- as.numeric(rownames(rf_data))

my_lines <- data.frame(matrix(nrow=100, ncol=0))
my_lines[["Trees"]] = 1:100
for (i in c(1,4,7,9)) {
  index <- paste("rf__", i, sep = "")
  name <- paste("Max features:", i)
  my_lines[[name]] <- as.vector(my_models[[index]]$finalModel$err.rate[,1])
}

# For removing margin from axis
par(mgp=c(1.8, 0.6, 0))

matplot(
  my_lines[,1], my_lines[,-1], 
  type = "l", 
  col = config$theme$plots, 
  lty = c(3, 2, 1, 5), 
  lwd = 2,
  xlab = "Number of trees", 
  ylab = "OOB Error",
  panel.first = grid(),
  mar = c(0, 0, 0, 0)
)
title("Out-of-bag (OOB) Error", line = 0.7)

legend(
  "topright", 
  lty = c(3, 2, 1, 5), 
  lwd = 2, 
  col = config$theme$plots, 
  colnames(my_lines)[-1]
)

#########################################
# Cleaning global environment
rm(rf_data)
rm(my_lines)