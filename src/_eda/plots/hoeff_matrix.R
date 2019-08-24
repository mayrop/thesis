# Computes the matrix for Hoeffding's (1948) D statistics
matrix <- hazel::corr.hoeffding(
  as.matrix(all[,colnames(all) %in% predictors]), 
  all$response_binary
)

offset_x <- c(1.8, 1.7, 1.9, 1.4, 1.7, 1.4, 1.3, 0.8, 1.4, -1.8, 1.5, -1.4, -1.40, -1.65, -1.25)
offset_y <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, rep(0, times = 4))

plot(
  matrix$hoeffding_d_rank, 
  matrix$spearman_rank,
  pch = 19,
  xlab = "Hoeffding's D Rank",
  ylab = "Spearman's Rank",
  main = "Scatter Plot of Spearman vs Hoeffding's D Ranks",
  panel.first = grid(
    col = "#d1d1d1"
  ),
  col = config$theme$parties_colors[1]
)

text(
  matrix$hoeffding_d_rank + offset_x, 
  matrix$spearman_rank + offset_y, 
  labels = rownames(matrix), 
  cex = 0.8
)

#########################################
# Cleaning global environment
rm(offset_x)
rm(offset_y)
