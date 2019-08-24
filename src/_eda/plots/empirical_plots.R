# Empirical Plots
empirical_plot <- emplogit(all, predictor, "response_binary")

x <- `if` (binned, empirical_plot$bin, empirical_plot$var)
xlab <- `if` (binned, paste("Bin # for ranked", predictor, "variable"), config$predictors$list[[predictor]]$name)
title <- `if` (binned, paste("Empirical Logit against binned", predictor, "variable"), paste("Empirical Logit against", predictor, "variable"))

plot(
  x = x, 
  y = empirical_plot$elogit, 
  main = title,
  type = "o",
  xlab = xlab,
  ylab = "Empirical Logit",
  pch = 19, 
  cex = 0.5,
  panel.first = grid()
)

lines(
  loess.smooth(x, empirical_plot$elogit), 
  col="red"
)

#########################################
# Cleaning global environment
rm(empirical_plot)
rm(x)
rm(xlab)
rm(title)
