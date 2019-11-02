# Empirical Plots
empirical_plot <- hazel::plots.emplogit(all, predictor, "response_binary")

x <- `if` (binned, empirical_plot$bin, empirical_plot$var)
xlab <- `if` (binned, paste("Bin # for ranked", predictor, "variable"), config$predictors$list[[predictor]]$name)
title <- `if` (binned, paste("Empirical Logit against binned", predictor), paste("Empirical Logit against", predictor))

par(mgp = c(2.5, 1, 0))

plot(
  x = x, 
  y = empirical_plot$elogit, 
  type = "o",
  xlab = xlab,
  ylab = "Empirical Logit",
  pch = 19, 
  cex = 0.5,
  panel.first = grid(
    col = "#d1d1d1"
  )
)
title(title, line = 0.7)

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
