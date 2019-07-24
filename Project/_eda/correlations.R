
correlations <- cor(all[,which(colnames(all) %in% predictors)])
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot::corrplot(correlations, order = "hclust", addrect = 10, tl.col = "black", tl.srt = 45)

### Cluster of variables

continous <- all[,-which(colnames(all) %in% category_cols)]
continous <- continous[,-which(names(continous) %in% elections_cols)]
continous <- continous[,-which(grepl("_07", names(continous)))]

correlation <- cor(continous, use="complete.obs", method="pearson")
correlation[correlation < 0.5] = 0

dissimilarity = 1 - correlation

dissimilarity <- as.data.frame(dissimilarity) %>%
  rownames_to_column('mycol') %>%
  filter(!(rowSums(dissimilarity) == nrow(dissimilarity) - 1)) %>%
  column_to_rownames('mycol') %>%
  select(rownames((dissimilarity[!(rowSums(dissimilarity) == nrow(dissimilarity) - 1),])))

distance = as.dist(dissimilarity)
cluster = hclust(distance, method="complete")
plot(cluster, cex=0.7)

rect.hclust(cluster, k = 15, border = 2:5)

# using dendrogram objects
hcd = as.dendrogram(cluster, hang=0.05) %>% 
  set("branches_k_color", k=13)

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

# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x[1:12], y=y, plot="density", scales=scales)
# featurePlot(x=x[13:24], y=y, plot="density", scales=scales)
# featurePlot(x=x[25:36], y=y, plot="density", scales=scales)
# featurePlot(x=log(x[37:48]), y=y, plot="density", scales=scales)
# featurePlot(x=log(x[49:50]), y=y, plot="density", scales=scales)


