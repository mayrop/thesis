# Empirical Plots
empirical_plot <- emplogit(all, covariate, "response_binary")

if (binned) {
  plot(empirical_plot$bin, empirical_plot$elogit, type="o")  
} else {
  plot(empirical_plot$var, empirical_plot$elogit, type="o")  
}

#########################################
# Cleaning global environment
rm(empirical_plot)
