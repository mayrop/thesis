library(RColorBrewer)

pattern <- paste(config$predictors$valid_suffixes, collapse="|")
importants <- filterVarImp(x = all[,which(grepl(pattern, names(all)))], y = all$response_factor)

importants <- importants[order(-importants$yes),]
importants <- importants[1:length(predictors),]
importants_names <- rownames(importants)

featurePlot(
  x=as.data.frame(train.data[, which(colnames(all) %in% predictors[predictors != "response_regression"])]), 
  y=pull(train.data[, which(colnames(all) %in% "response_factor")]), 
  plo ="density",
  scales=list(x=list(relation="free"), y=list(relation="free")),
  strip=strip.custom(par.strip.text=list(cex=.7)),
  auto.key=list(space="top", columns=2, cex.title=1)
)

temp_predictors <- predictors
temp_predictors[temp_predictors=="response_regression"] <- "frac_republican"

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
correlations <- cor(all[,which(colnames(all) %in% temp_predictors)])

png(filename="figures/corrplot.png", width=450, height=260, bg="white", unit="mm", res=300)
corrplot::corrplot(
  correlations,
  order="hclust",
  hclust.method="complete",
  # careful with this
  tl.col=c(
    rep("black", 2),
    "#92000a",
    rep("black", 13)
  ), 
  tl.srt=45,
  #tl.pos="ld",
  addrect=11,
  col=brewer.pal(n=10, name="RdYlBu"),
  win.asp=.5
)

title("Correlation Matrix between frac_republican and covariates", line=2, font=24)
dev.off()

# heatmap(x = correlations,  symm = TRUE)

### Cluster of variables
regex <- paste(config$predictors$valid_suffixes, collapse="|")

continous <- all[,-which(colnames(all) %in% category_cols)]
continous <- continous[,-which(names(continous) %in% elections_cols)]
continous <- continous[,which(grepl(regex, names(continous)))]

correlation <- cor(continous, use="complete.obs", method="pearson")
correlation[correlation < 0.55] = 0

dissimilarity <- 1 - correlation
dissimilarity <- as.data.frame(dissimilarity) %>%
  rownames_to_column('mycol') %>%
  filter(!(rowSums(dissimilarity) == nrow(dissimilarity) - 1)) %>%
  column_to_rownames('mycol') %>%
  dplyr::select(rownames((dissimilarity[!(rowSums(dissimilarity) == nrow(dissimilarity) - 1),])))

distance = as.dist(dissimilarity)
cluster = hclust(distance, method="complete")
plot(cluster, cex=0.7)

rect.hclust(cluster, k = 15, border = 2:5)

# using dendrogram objects
hcd = as.dendrogram(cluster, hang=0.05) %>% 
  dendextend::set("branches_k_color", k=10)

# alternative way to get a dendrogram
plot(hcd, horiz=T, xlim=c(1, -0.5))

##############################################
##############################################

# TODO - Check later
# exclude <- unique(c(
  # which(colnames(all) %in% category_cols[category_cols != "party_won"]),
  # which(colnames(all) %in% elections_cols[elections_cols != "party_won"]) 
# ))

# pairs_matrix <- all[,-c(exclude)]

# pairs(pairs_matrix[,c(1:7)], col=pairs_matrix$party_won)
# pairs(pairs_matrix[,c(8:12)], col=pairs_matrix$party_won)
# pairs(pairs_matrix[,c(13:17)], col=pairs_matrix$party_won)
# pairs(pairs_matrix[,c(20:24)], col=pairs_matrix$party_won)

# x <- pairs_matrix[,-c(1)]
# y <- pairs_matrix[,]$party_won

## load the data
# data(iris)
## pair-wise plots of all 4 attributes, dots colored by class
# featurePlot(x=iris[,1:4], y=iris[,5], plot="pairs", auto.key=list(columns=3))




