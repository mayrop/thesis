###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
### 

# D A T A

# Setting project directory
# setwd("~/Github/thesis/Project")
# setwd("H:/thesis-master/Project")
library(config)
config <- config::get()

source("_settings/functions.R")
load_libraries(config$settings$libraries)
load_sources(config$settings$sources)

datasets <- load_datasets(config$settings$data_folder)

# S E T T I N G S

source("_settings/theme.R")

############################
############################

# D A T A . P R E P R O C E S S I N G

# Here we fix some data inconsistencies
source("_data_preprocessing/data_fixes.R")

# Here we process elections dataset
source("_data_preprocessing/elections.R")

# Here we process facts dataset
source("_data_preprocessing/facts.R")

## Sanity checks
source("_data_preprocessing/sanity_checks.R")

## Joins
source("_data_preprocessing/joins.R")

############################################
############################################

set.seed(config$seed)
response <- config$predictors$response_variable

indices = createDataPartition(all$response_binary, p = 0.6, list = FALSE)
train.data = all[indices,]
test.data = all[-indices,]

rm(indices)

# Double checking proportions

prop.table(table(train.data$response_binary))
table(train.data$response_binary)

prop.table(table(test.data$response_binary))
table(test.data$response_binary)

############################################
############################################

# Exploratory Data Analysis

## Joins
source("_eda/predictors.R")
length(predictors)
predictors

source("_eda/correlations.R")

# source("_eda/plots.R") # TODO check & improve
source("_eda/maps.R")


#####################################

