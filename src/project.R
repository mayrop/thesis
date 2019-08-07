###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
### 

# S E T T I N G S

# Setting project directory
# setwd("~/Github/thesis/Project")
# setwd("H:/thesis-master/Project")
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

# Exploratory Data Analysis

## Joins
source("_eda/predictors.R")
length(predictors)
predictors

source("_eda/correlations.R")

# source("_eda/plots.R") # TODO check & improve
# source("_eda/maps.R")

############################################
############################################

# Data Modelling

# Loads custom functions for models
source("_models/functions.R")

# Loads all the setup for the methods to compare
source("_models/methods.R")

# Builds all the models.. (be patient)
source("_models/build.R")

# https://stackoverflow.com/questions/48079660/extract-the-coefficients-for-the-best-tuning-parameters-in-caret
# coef(models[["glmnet"]]$finalModel, models[["glmnet"]]$finalModel$lambdaOpt)
