# Computes the matrix for Hoeffding's (1948) D statistics
matrix <- hazel::corr.hoeffding(
  as.matrix(all[,colnames(all) %in% predictors]), 
  all$response_binary
)

offset_x <- c(1.5, rep(0, times=length(predictors)-3), -1, -1.3)
offset_y <- c(0, rep(0.4, times=length(predictors)-3), rep(0, times=2))

plot(
  matrix$hoeffding_d_rank, 
  matrix$spearman_rank,
  pch = 19,
  xlab = "Hoeffding's D Rank",
  ylab = "Spearman's Rank",
  main = "Scatter Plot of Spearman vs Hoeffding's D Ranks",
  panel.first = grid()
)

text(
  matrix$hoeffding_d_rank + offset_x, 
  matrix$spearman_rank + offset_y, 
  labels = rownames(matrix), 
  cex = 0.85
)

#########################################
# Cleaning global environment
rm(matrix)
rm(offset_x)
rm(offset_y)
