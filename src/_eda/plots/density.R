# Here we plot the complete density plots

featurePlot(
  # to change the order of plots
  as.table = TRUE,
  # setting x & y
  x = as.data.frame(all %>% ungroup() %>% select(plot_vars)), 
  y = pull(all[, which(colnames(all) %in% "response_factor")]), 
  plo = "density",
  # theme settings
  pch = 16, # what's the figure for the density
  cex = 0.02,  # what's the size for the figure
  col = config$theme$parties_colors,
  lty = c(4, 1),
  par.settings = list(
    strip.background = list(col = "#f3f3f3")
  ),
  strip = strip.custom(par.strip.text = list(cex = .6)),
  # scales for each plot axis
  scales = list(
    x = list(relation="free"), 
    y = list(relation="free")
  ),
  main = "Density Plots for Predictors",
  # legend
  key = list(
    space = "bottom", 
    columns = 2,
    text = list(rev(config$theme$parties_labels)),
    lines = list(
      col = rev(config$theme$parties_colors),
      lty = c(1, 4)
    ),
    cex.title = 1, 
    cex = .9,
    title = "Winning Candidate"
  ),
  layout = plot_layout,
  labels = c("", "Density")
)

#########################################
# Cleaning global environment
rm(plot_vars)
rm(plot_layout)


