############################

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

temp_predictors <- predictors
temp_predictors[temp_predictors=="response_regression"] <- "frac_republican"

correlations <- cor(all[,which(colnames(all) %in% temp_predictors)])

corrplot::corrplot(
  correlations,
  order="hclust",
  hclust.method="complete",
  # careful with this
  tl.col=c(
    rep("black", 2),
    "red",
    rep("black", 13)
  ), 
  tl.srt = 45,
  addrect = 11,
  col = config$theme$correlations,
  win.asp = .5
)

##############################################
##############################################
