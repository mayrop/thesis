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

 "GGally", "gridExtra", 
"moderndive", "skimr", "plotly",  "magrittr", 
"tidyverse", "reshape2", "caret", "ellipse", "car", "rcompanion", 
"e1071", "pROC", "glmnet", 
"corrplot", "dendextend", "cowplot",  
"sf", 
"digest",
"data.table",
"LOGIT",
"doParallel"
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

# C o r r e l a t i o n P l o t s

execute_and_plot("_eda/plots/density.R", 
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)

execute_and_plot("_eda/plots/corrplot.R", 
  save=config$print, width=250, height=180, prefix=config$settings$images_folder
)


# png(filename="figures/corrplot.png", width=450, height=260, bg="white", unit="mm", res=300)
source("_eda/correlations.R")
dev.off()

# M a p s 

config$print & png(filename="figures/map_binary.png", width=250, height=180, bg="transparent", unit="mm", res=300)
source("_eda/maps/binary.R", print.eval=config$print)
config$print & dev.off()

config$print & png(filename="figures/map_votes.png", width=250, height=180, bg="transparent", unit="mm", res=300)
source("_eda/maps/votes.R", print.eval=config$print)
config$print & dev.off()

# source("_eda/plots.R") # TODO check & improve
# source("_eda/maps.R")

############################################
############################################

# Data Modelling

# Splitting data
source("_models/split.R")

# Loads custom functions for models
source("_models/functions.R")

# Loads all the setup for the methods to compare
source("_models/methods.R")

# Builds all the models.. (be patient)
source("_models/build.R")


sink("output/sessionInfo.txt", append=FALSE, split=TRUE)
sessionInfo()
sink()
