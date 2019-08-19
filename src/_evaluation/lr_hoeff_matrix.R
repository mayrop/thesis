# Computes the matrix for Hoeffding's (1948) D statistics
matrix <- hazel::corr.hoeffding(
  as.matrix(all[,colnames(all) %in% predictors]), 
  all$response_binary
)

plot(
  matrix$hoeffding_d_rank, 
  matrix$spearman_rank
)

text(
  matrix$hoeffding_d_rank + 0.45, 
  matrix$spearman_rank + 0.45, 
  labels = paste(rownames(m), m$spearman_rank, m$hoeffding_d_rank), 
  cex = 0.4
)

#########################################
# Cleaning global environment
rm(matrix)
