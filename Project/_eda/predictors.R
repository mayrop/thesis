##############################################
##############################################

types <- sapply(all, class)
cols <- names(types)

category_cols <- cols[types=="character"|types=="factor"]
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
  filter(column==config$predictors$regression_variable | row==config$predictors$regression_variable) %>% 
  filter(
    !(column %in% elections_cols & column != config$predictors$regression_variable) & 
      !(row %in% elections_cols & row != config$predictors$regression_variable)) %>% 
  mutate(
    cor_abs = abs(cor), 
    var = ifelse(row == config$predictors$regression_variable, as.character(column), as.character(row))
  ) %>% 
  arrange(desc(cor_abs)) %>% 
  select(var, cor, p, cor_abs)

# Filtering out variables that are not highly correlated
predictors <- elections_correlation_table[elections_correlation_table$cor_abs > config$predictors$correlation,]
predictors <- predictors$var

# Adding the regression variable to the predictos vector
# TODO - Check & document why
predictors <- c(predictors, config$predictors$regression_variable)

# Removing 2007 variables if needed
if (config$predictors$remove_2007) {
  predictors <- predictors[!grepl("_07",predictors)]  
}

