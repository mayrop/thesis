
key.trans <- list(
  space="bottom", 
  columns=2,
  text=list(config$theme$parties_labels),
  lines=list(col=config$theme$parties_colors),
  cex.title=1, 
  cex=.9,
  title="Winning Candidate"
)

featurePlot(
  # to change the order of plots
  index.cond=list(c((length(predictors)-1):1,NULL)),
  x=as.data.frame(all[, which(colnames(all) %in% predictors[predictors != "response_regression"])]), 
  y=pull(all[, which(colnames(all) %in% "response_factor")]), 
  plo="density",
  # theme settings
  pch=16, # what's the figure for the density
  cex=0.2,  # what's the size for the figure
  col=config$theme$parties_colors,
  par.settings=list(
    strip.background=list(col="#f3f3f3")
  ),
  strip=strip.custom(par.strip.text=list(cex=.6)),
  # titles
  labels=c(),
  # scales for each plot axis
  scales=list(x=list(relation="free"), y=list(relation="free")),
  # legend
  key=key.trans,
  layout=c(5, 3)
)


