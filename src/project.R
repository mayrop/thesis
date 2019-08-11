###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
#We make use of the following R packages: `tidyverse`, for data manipulation and visualization,
#corrplot, for creating correlation plots, caret, for our modelling framework including data-splitting, 
#model tuning and evaluation, pROC for ROC curve visualization, doParallel, for parallalizatin in our traning models
#sf, a package that standardizes a way to encode spatial vector data
#pROC
#doParallel
#"UrbanInstitute/urbnmapr"

### 

# S E T T I N G S

# Setting project directory
# setwd("~/Github/thesis/src")
print("Loading config")
library(config)
config <- config::get()

# Getting started...
source("init.R")

############################################
############################################

# D a t a . P r e p r o c e s s i n g

# Here we fix some data inconsistencies
source("_data/fix.R")

# Here we process elections dataset
source("_data/elections.R")

# Here we process facts dataset
source("_data/facts.R")

# Sanity checks
source("_data/sanity_checks.R")

# Joins
source("_data/joins.R")

############################################
############################################

# E x p l o r a t o r y . D a t a . A n a l y s i s 

## Joins
source("_eda/predictors.R")
length(predictors)
predictors


############################################
# Function for saving
execute_and_plot <- function(file, save=FALSE, prefix="", ext=".png", bg="white", unit="mm", res=300, ...) {
  filename <- gsub("/", "_", file)
  filename <- gsub("^_", "", filename)
  filename <- gsub("\\.\\w+$", "", filename)  
  filename <- paste(prefix, filename, ext, sep="")
  
  save & png(filename=filename, bg=bg, unit=unit, res=res, ...)
  source(file, print.eval=save)
  save & dev.off()
}

# C o r r e l a t i o n . P l o t s

execute_and_plot("_eda/plots/density.R", 
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)

execute_and_plot("_eda/plots/corrplot.R", 
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)

# M a p s 

execute_and_plot(
  "_eda/maps/binary.R",
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)

execute_and_plot(
  "_eda/maps/votes.R",
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)

############################################
############################################

# D a t a . M o d e l l i n g

#--------------------------------------------------------#
# Start parallelization...
# https://www.rdocumentation.org/packages/parallel/versions/3.6.1/topics/makeCluster
cores <- parallel::detectCores()
if (cores > 3) {
  # makePSOCKcluster enhanced version of makeCluster
  # leave enough cores for other tasks
  clusters <- makePSOCKcluster(cores - 2)
  registerDoParallel(clusters)
}
#--------------------------------------------------------#

# Define our base control for caret
# This is just for the base so the models can extend ti
base_control <- trainControl(
  method = "cv",
  number = 5, 
  summaryFunction = function(...) { 
    # this is an overwrite of twoClassSummary from caret
    # this is to prevent sens & spec to be switched
    two_class_summary(...)
  }, 
  classProbs = TRUE, 
  sampling = "up",
  allowParallel = FALSE
)

# This is just to build our main formula for building the diff models
my_formula <- as.formula(paste(
  "response_factor ~",
  paste(predictors[predictors != "response_regression"], collapse = " + ")
))

# Global variables that will 
my_models <- list()
my_resamples <- list()
my_metrics <- list()
my_methods <- list()

# this is for ROC
my_levels <- rev(levels(test.data$response_factor))

# Splitting data
source("_models/split.R")

# Loads custom functions for models
source("_models/functions.R")

# Training...
source("_models/glm.R")
source("_models/rf.R")
source("_models/svmLinear.R")
source("_models/svmPoly.R")
source("_models/svmRadial.R")






#--------------------------------------------------------#
# Stop paralellization
stopCluster(cl)
registerDoSEQ()
rm(cl)
#--------------------------------------------------------#



sink("output/sessionInfo.txt", append=FALSE, split=TRUE)
sessionInfo()
sink()
