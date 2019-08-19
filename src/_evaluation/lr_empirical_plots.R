# Empirical Plots
empirical_plot <- emplogit(all, predictor, "response_binary")

x <- `if` (binned, empirical_plot$bin, empirical_plot$var)
xlab <- `if` (binned, paste("Bins for ", predictor), predictor)

plot(
  x, 
  empirical_plot$elogit, 
  type = "o",
  xlab = xlab,
  ylab = "Logit"
)

#########################################
# Cleaning global environment
rm(empirical_plot)
rm(x)
rm(xlab)
