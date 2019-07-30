####################################################
# Loading 
load_libraries <- function(libraries) {
  for (library in libraries) {
    if (!require(library, character.only = TRUE)) {
      install.packages(library)
    }
    
    library(library, character.only = TRUE)
  }  
}

load_sources <- function(sources) {
  for (s in sources) {
    source(s)
  }  
}

load_datasets <- function(folder = "data") {
  datasets <- list()
  
  for (file in list.files(folder)) {
    path <- paste(folder, "/", file, sep="")
    name <- gsub("\\.\\w+$", "", file)
    
    datasets[[name]] <- read.csv(path)
  } 
  
  return(datasets)
}

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

get_map_color <- function(lead_party, lead_votes) {
  if (lead_votes <= 0.1) {
    val <- 1
  } else if (lead_votes <= 0.2) {
    val <- 2
  } else if (lead_votes <= 0.3) {
    val <- 3
  } else {
    val <- 4  
  }
  
  if (lead_party == "democrat") {
    val <- val + 4
  }
  
  if (lead_party %in% c("democrat", "republican")) {
    return(val)
  }
  
  stop("error happened")
}




my_model <- function(formula, dataset, threshold = 0.05) {
  response <- all.vars(formula)[1]
  
  model <- glm(
    formula, data=dataset, family=binomial()
  )
  coefficients <- coef(summary(model))[,4]
  
  keep <- data.frame(
    covariate = names(coefficients),
    p_value = coefficients
  )
  
  keep <- keep[order(-keep$p_value),]
  keep <- keep[keep$covariate!="(Intercept)",]
  
  if (keep[1, "p_value"] <= threshold) {
    return(
      list(
        formula=formula,
        model=model
      )
    )
  }
  
  keep <- keep[-1,]
  
  formula <- build_initial_formula(
    response=response,
    predictors=as.vector(rev(keep$covariate))
  )
  
  return(
    my_model(
      formula=formula,
      dataset=dataset
    )
  )
}