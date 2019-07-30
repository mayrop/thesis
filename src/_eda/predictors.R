##############################################
##############################################

# pattern <- paste(config$predictors$valid_suffixes, collapse="|")

# importants <- filterVarImp(x = all[,which(grepl(pattern, names(all)))], y = pull(all[,response]))

# importants <- importants[order(-importants$yes),]
# importants <- importants[1:20,]
# importants_names <- rownames(importants)

# setdiff(importants_names, predictors)
# setdiff(predictors, importants_names)

# knnFit <- train(as.data.frame(all[,which(grepl(pattern, names(all)))]), pull(all[,response]), "knn")
# knnImp <- varImp(knnFit)
# dotPlot(knnImp)
# }

# featurePlot(x=x[1:12], y=y, plot="density", )
# 

types <- sapply(all, class)
cols <- names(types)

category_cols <- cols[types=="character" | types=="factor"]
elections_cols <- cols[grepl("votes_|frac_|party_|prop_", cols)]

continous <- train.data[,-which(colnames(train.data) %in% category_cols)]

if (config$predictors$use_all_for_correlations) {
  continous <- all[,-which(colnames(all) %in% category_cols)]  
}

demographic_correlations_table <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
  filter(!(column %in% elections_cols) & !(row %in% elections_cols)) %>% 
  # Only interested in the cols correlated to prop_republican
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

elections_correlation_table <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
  # Only interested in the cols correlated to prop_republican
  filter(column=="response_regression" | row=="response_regression") %>% 
  filter(
    !(column %in% elections_cols & column != "response_regression") & 
      !(row %in% elections_cols & row != "response_regression")) %>% 
  mutate(
    cor_abs = abs(cor), 
    var = ifelse(row == "response_regression", as.character(column), as.character(row))
  ) %>% 
  arrange(desc(cor_abs)) %>% 
  dplyr::select(var, cor, p, cor_abs)

# Filtering out variables that are not highly correlated
predictors <- elections_correlation_table[elections_correlation_table$cor_abs >= config$predictors$correlation,]
predictors <- predictors$var

# 
regex <- paste(config$predictors$valid_suffixes, collapse="|")
predictors <- predictors[grepl(regex,predictors)]  


# Adding the regression variable to the predictos vector
# TODO - Check & document why
predictors <- c(predictors, c("response_regression"))

#################################################################################
######### Cleanup
rm(regex)

