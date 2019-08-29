# Here we do the correlation plot

corrplot::corrplot(
  cor(all[,which(colnames(all) %in% predictors)]),
  order = "hclust",
  hclust.method = "complete",
  tl.srt = 50,
  addrect = 11,
  col = config$theme$correlations,
  win.asp = .5,
  tl.col = "black",
  tl.pos = "lt",
  mar = c(0, 0, 1, 1),
  tl.cex = 1.5
)

##############################################
