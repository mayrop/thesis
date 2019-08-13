###
#
# University of Glasgow
# Thesis for MSc in Data Analytics Degree
# August, 2019
#
# Title: 
#   Assessing the impact of socio-economic factors on 
#   Presidential Primary Election voting in the USA in 2016
#
# Author: Mayra A. Valdes I @mayrop
#
### 

# S e t t i n g s

# Setting project directory
# setwd("~/Github/thesis/src")
print("Loading config...")
library(config)
config <- config::get()

# Getting started...
source("init.R")

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

# Merging both datasets
source("_data/joins.R")

# Picking variables for the analysis...
source("_eda/predictors.R")

############################################

# E x p l o r a t o r y . D a t a . A n a l y s i s 

# M a p s 

execute_and_plot(
  "_eda/maps/binary.R",
  save = config$print, 
  width = 250, 
  height = 180, 
  prefix = config$settings$images_folder
)

execute_and_plot(
  "_eda/maps/votes.R",
  save = config$print, 
  width = 250, 
  height = 180, 
  prefix = config$settings$images_folder
)

# D e n s i t y . P l o t s

# Here we plot interesting predictors
plot_layout <- c(5, 1)
plot_vars <- c(
  "hsg_multiunit_pct_13", "rh_white_pct_14", "rh_white_nohisp_pct_14", 
  "edu_bach_pct_13", "hsg_val_units_13"
)
execute_and_plot(
  "_eda/plots/density.R", 
  save = config$print, 
  width = 300, 
  height = 75, 
  prefix = config$settings$images_folder
)

# Here we plot all the predictors (appendix)
plot_layout <- c(4, 4)
plot_vars <- c(
  "pop_14", "pop_10", "pop_density_10", "pop_foreign_pct_13",
  "pop_o_lang_pct_13", "age_o65_pct_14", "edu_bach_pct_13", "inc_pc_12_month_13", 
  "rh_white_pct_14", "rh_white_nohisp_pct_14", "rh_asian_pct_14", "rh_afroamerican_pct_14", 
  "hsg_multiunit_pct_13", "hsg_val_units_13", "hsg_homeowner_rate_13", "nf_priv_emplt_rate_13"
)
execute_and_plot(
  "_eda/plots/density.R", 
  save = config$print, 
  width = 250, 
  height = 250, 
  prefix = config$settings$images_folder,
  suffix = "_all"
)


# C o r r e l a t i o n . P l o t s

execute_and_plot("_eda/plots/corrplot.R", 
  save = config$print, 
  width = 400, 
  height = 200, 
  prefix = config$settings$images_folder
)


############################################

#--------------------------------------------------------#
# Start parallelization...
cores <- parallel::detectCores()
if (cores > 3) {
  # makePSOCKcluster enhanced version of makeCluster
  # leave enough cores for other tasks
  clusters <- makePSOCKcluster(cores - 2)
  registerDoParallel(clusters)
}
#--------------------------------------------------------#

# D a t a . M o d e l l i n g

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
  paste(predictors, collapse = " + ")
))

# Global variables that will 
my_models <- list()
my_resamples <- list()
my_metrics <- list()
my_methods <- list()

# Splitting data
source("_models/split.R")

# this is for ROC
my_levels <- rev(levels(test.data$response_factor))

# Loads custom functions for models
source("_models/functions.R")

# Training...
source("_models/glm.R")
source("_models/rf.R")
#source("_models/svmLinear.R")
#source("_models/svmPoly.R")
source("_models/svmRadial.R")

#--------------------------------------------------------#
# Stop paralellization
if (cores > 3) {
  stopCluster(clusters)
  registerDoSEQ()
  rm(clusters)
}
#--------------------------------------------------------#

############################################

# E v a l u a t i o n

execute_and_plot(
  "_evaluation/roc.R",
  save = config$print, 
  width = 200, 
  height = 200, 
  prefix = config$settings$images_folder
)

# Writing session info...
sink("output/sessionInfo.txt", append=FALSE, split=TRUE)
sessionInfo()
sink()

# Writing packages bibliography...
# write.bib(c(config$settings$libraries, c("config", "urbnmapr", "hazel")), file="r")

          