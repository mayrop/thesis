
libraries <- c(
  "dplyr", # for %>%
  "stringr", # for str_pad
  "skimr",
  "tidyr",
  "ggplot2",
  "ROCR",
  "glmnet", # regularized logistic regression
  "tibble", # for `rownames_to_column` and `column_to_rownames`
  "dendextend"
)

# Loading Libraries
libraries <- c(
  "ggplot2", "GGally", "gridExtra", "dplyr", 
  "moderndive", "skimr", "plotly", 
  "tidyr", "magrittr", "tidyverse", 
  "urbnmapr", "reshape2", "corrplot", "caret",
  "ellipse", # https://stackoverflow.com/questions/44502469/r-featureplot-returning-null
  # "psych", "emmeans", "lmtest", # https://rcompanion.org/handbook/J_02.html
  "car", "rcompanion", "e1071", "ROCR", "glmnet", "tibble", "dendextend",
  "sf", "cowplot",  # maps
  #"extrafont",
  "stringi" #https://stackoverflow.com/questions/29265172/print-unicode-character-string-in-r
)


# https://www.datacamp.com/community/tutorials/logistic-regression-R
source("http://www.sthda.com/upload/rquery_cormat.r")

# devtools::install_github("UrbanInstitute/urbnmapr")

for (library in libraries) {
  if (!require(library, character.only = TRUE)) {
    install.packages(library)
  }
  
  library(library, character.only = TRUE)
}