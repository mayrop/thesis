
key.trans <- list(
  space="bottom", 
  columns=2,
  text=list(config$theme$parties_labels),
  lines=list(col=config$theme$parties_colors),
  cex.title=1, 
  cex=.9,
  title="Winning Party"
)

# png(filename="figures/density_plots.png", width=300, height=200, bg="white", unit="mm", res=300)

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
  main="Density plots for each covariate and winning party",
  labels=c(),
  # scales for each plot axis
  scales=list(x=list(relation="free"), y=list(relation="free")),
  # legend
  key=key.trans,
  layout=c(5, 4)
)

# dev.off()

############################

temp_predictors <- predictors
temp_predictors[temp_predictors=="response_regression"] <- "frac_republican"

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
correlations <- cor(all[,which(colnames(all) %in% temp_predictors)])

# png(filename="figures/corrplot.png", width=450, height=260, bg="white", unit="mm", res=300)
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
  tl.srt=45,
  #tl.pos="ld",
  addrect=11,
  col=config$theme$correlations,
  win.asp=.5
)

title("Correlation Matrix between frac_republican and covariates", line=2, font=24)
# dev.off()

### Cluster of variables
#regex <- paste(config$predictors$valid_suffixes, collapse="|")

#continous <- all[,-which(colnames(all) %in% category_cols)]
#continous <- continous[,-which(names(continous) %in% elections_cols)]
#continous <- continous[,which(grepl(regex, names(continous)))]

#correlation <- cor(continous, use="complete.obs", method="pearson")
#correlation[correlation < 0.55] = 0

#dissimilarity <- 1 - correlation
#dissimilarity <- as.data.frame(dissimilarity) %>%
#  rownames_to_column('mycol') %>%
#  filter(!(rowSums(dissimilarity) == nrow(dissimilarity) - 1)) %>%
#  column_to_rownames('mycol') %>%
#  dplyr::select(rownames((dissimilarity[!(rowSums(dissimilarity) == nrow(dissimilarity) - 1),])))

#distance = as.dist(dissimilarity)
#cluster = hclust(distance, method="complete")
#plot(cluster, cex=0.7)

#rect.hclust(cluster, k = 15, border = 2:5)

# using dendrogram objects
#hcd = as.dendrogram(cluster, hang=0.05) %>% 
#  dendextend::set("branches_k_color", k=10)

# alternative way to get a dendrogram
#plot(hcd, horiz=T, xlim=c(1, -0.5))

##############################################
##############################################
