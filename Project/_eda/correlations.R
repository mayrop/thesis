##############################################
##############################################

types <- sapply(all, class)
cols <- names(types)

# 11 category cols   
category_cols <- cols[types=="character"|types=="factor"]
elections_cols <- cols[grepl("votes_|frac_|party_|prop_", cols)]

train_continous <- train.data[,-which(colnames(train.data) %in% category_cols)]
continous <- all[,-which(colnames(all) %in% category_cols)]

demographic_correlations_table <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
  filter(!(column %in% elections_cols) & !(row %in% elections_cols)) %>% 
  # Only interested in the cols correlated to prop_republican
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

elections_correlation_table <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
  # Only interested in the cols correlated to prop_republican
  filter(column=="prop_republican" | row=="prop_republican") %>% 
  filter(
    !(column %in% elections_cols & column != "prop_republican") & 
      !(row %in% elections_cols & row != "prop_republican")) %>% 
  mutate(
    cor_abs = abs(cor), 
    var=ifelse(row == "prop_republican", as.character(column), as.character(row))
  ) %>% 
  arrange(desc(cor_abs)) %>% 
  select(var, cor, p, cor_abs)

# Filtering out variables that are not highly correlated
predictors <- elections_correlation_table[elections_correlation_table$cor_abs > 0.2,]
predictors <- predictors$var

predictors <- c(predictors, "prop_republican")

# TODO - Backup the idea of removing _07 variables
# predictors <- predictors[!grepl("_07",predictors)]
# correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])

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


