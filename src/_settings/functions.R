####################################################

# M o d e l s

build_initial_formula <- function(response, predictors=c(), regex="", transformations=list()) {
  formula <- ""
  
  for (predictor in predictors) {
    # needs to have one of the following regex to be valid
    if (regex != "" & !grepl(regex, predictor)) {
      next
    }
    
    if (!(predictor %in% names(transformations))) {
      transformations[[predictor]] <- predictor
    }
    
    sep <- ifelse(formula == "", " ", " + ")
    formula <- paste(formula, transformations[[predictor]], sep=sep)
  }
  
  return(as.formula(paste(response, " ~ ", formula, sep="")))
}

#######################################################################

plot_party_won_boxplot = function(var) {
  ggplot(all, aes_string(
    x = "party_won", y = (var))) +
    geom_boxplot()
}

########################################################################
