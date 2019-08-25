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

# Global variables that will help us throughtout the project
my_models <- list()
my_resamples <- list()
my_methods <- list()
my_results <- list(
  "train" <- list(),
  "test" <- list(),
  "contigencies" = list()
)


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
predictors <- names(config$predictors$list)
#predictors <- colnames(facts)[!(colnames(facts) %in% c(
#  "fips", "area_name", "state_facts", "pop_pct_change", "pop_10", "pop_density_10"
#))]

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

for (predictor in names(config$predictors$list)) {
  execute_and_plot(
    "_eda/plots/density.R", 
    save = config$print, 
    width = 150, 
    height = 140, 
    prefix = config$settings$images_folder,
    suffix = paste("_", predictor, sep="")
  )
  
  # cleaning...
  rm(predictor)
}

# L i n e a r i t y . A s s u m p t i o n

# Coefficients
execute_and_plot(
  "_eda/plots/hoeff_matrix.R",
  save = config$print, 
  width = 200, 
  height = 140, 
  prefix = config$settings$images_folder
)

# Empirical plots
for (predictor in names(config$predictors$list)) {
  for (binned in c(TRUE, FALSE)) {
    suffix <- `if`(binned, "binned", "")
    
    execute_and_plot(
      "_eda/plots/empirical_plots.R", 
      save = config$print, 
      width = 150, 
      height = 140, 
      prefix = config$settings$images_folder,
      suffix = paste("_", predictor, "_", suffix, sep="")
    ) 
  }
  
  # cleaning...
  rm(predictor)
  rm(binned)
  rm(suffix)
}

# C o r r e l a t i o n . P l o t

execute_and_plot("_eda/plots/corrplot.R", 
  save = config$print, 
  width = 400, 
  height = 200, 
  prefix = config$settings$images_folder
)

#--------------------------------------------------------#

# D a t a . M o d e l l i n g

# This is just to build our main formula for building the diff models

# Splitting data
source("_models/split.R")

# Training...
for (sampling in c("up", "")) {
  source("_models/glm.R")
  source("_models/rf.R")
  source("_models/svm.R")
}

#--------------------------------------------------------#

# E v a l u a t i o n
execute_and_plot(
  "_evaluation/lr_odds.R",
  save = config$print, 
  width = 200, 
  height = 130, 
  prefix = config$settings$images_folder
)

execute_and_plot(
  "_evaluation/roc.R",
  save = config$print, 
  width = 105, 
  height = 105, 
  prefix = config$settings$images_folder
)

execute_and_plot(
  "_evaluation/tuning_rf.R",
  save = config$print, 
  width = 110, 
  height = 110, 
  prefix = config$settings$images_folder
)

execute_and_plot(
  "_evaluation/tuning_svm.R",
  save = config$print, 
  width = 90, 
  height = 90, 
  prefix = config$settings$images_folder
)

execute_and_plot(
  "_evaluation/rf_errors.R",
  save = config$print, 
  width = 110, 
  height = 110, 
  prefix = config$settings$images_folder
)

# Writing session info...
sink("output/sessionInfo.txt", append=FALSE, split=TRUE)
sessionInfo()
sink()

# Writing packages bibliography...
# write.bib(c(config$settings$libraries, c("config", "urbnmapr", "hazel")), file="r")

          