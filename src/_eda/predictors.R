# Getting the predictors we are going to use...

types <- sapply(all, class)
cols <- names(types)

category_cols <- cols[types=="character" | types=="factor"]
elections_cols <- cols[grepl("votes_|frac_|party_|prop_", cols)]

continous <- all[,-which(colnames(all) %in% category_cols)]  

demographic_corr_tbl <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
  filter(!(column %in% elections_cols) & !(row %in% elections_cols)) %>% 
  # Only interested in the cols correlated to prop_republican
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

elections_cor_tbl <- rquery.cormat(continous, type="flatten", graph=FALSE)$r %>% 
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
predictors <- elections_cor_tbl[
  elections_cor_tbl$cor_abs >= config$predictors$correlation,
]
predictors <- predictors$var

regex <- paste(config$predictors$valid_suffixes, collapse="|")
predictors <- predictors[grepl(regex,predictors)]  
length(predictors)
predictors

#########################################
# Cleaning global environment
rm(regex)

