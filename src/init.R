###
#
# In this file we initalize all the libraries and datasts
#
# @author Mayra Valdes @mayrop
# 
### 

##############################################
# L o a d i n g . h a z e l . p a c k a g e...
#
# https://github.com/mayrop/hazel
if (!require("hazel")) {
  print("Seems hazel package is not installed...")
  if (!require("devtools", character.only = TRUE)) {
    print("Seems devtools package is not installed... installing it now")
    install.packages("devtools")
  }
  
  library(devtools)
  print("Installing hazel package from github")
  devtools::install_github("mayrop/hazel")
}

###################################
# L o a d i n g . d a t a s e t s
#
print("Loading libraries")
load.libraries(config$settings$libraries)

print("Loading libraries from source")
load.sources(config$settings$sources)

print("Loading libraries from github")
load.github(config$settings$github)

print("Loading datasets")
datasets <- load.datasets(config$settings$data_folder)

###################################
