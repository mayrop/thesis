###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
### 
# https://www.datacamp.com/community/tutorials/logistic-regression-R


# Loading Libraries
libraries <- c(
  "ggplot2", "GGally", "gridExtra", "dplyr", 
  "moderndive", "skimr", "plotly", "tidyr", "tidyverse", 
  "urbnmapr", "reshape2", "corrplot", "caret",
  "ellipse", # https://stackoverflow.com/questions/44502469/r-featureplot-returning-null
  "psych", "betareg", "emmeans", "lmtest", # https://rcompanion.org/handbook/J_02.html
  "car", "rcompanion", "e1071", "sf", "ROCR"
)

libraries <- c(
  "dplyr", # for %>%
  "stringr", # for str_pad
  "skimr",
  "tidyr",
  "ggplot2",
  "ROCR"
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

# Setting project directory
# setwd("~/Github/thesis/Project")
# setwd("H:/Project")

# Reading datasets
elections_uni <- read.csv("data/counties-uni.csv")
elections_orig <- read.csv("data/counties-original.csv")
dictionary_orig <- read.csv("data/dictionary.csv")
facts_orig <- read.csv("data/facts.csv")

significant_p_value <- 0.05

# Changes per:
# https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
elections_orig[elections_orig$FIPS==46113 & !is.na(elections_orig$FIPS),]$FIPS <- 46102
facts_orig[facts_orig$fips==46113 &  !is.na(facts_orig$fips),]$fips <- 46102

# Source code for all the functions
source("_functions.R")

# Here we process elections dataset
source("_elections.R")

# Here we process facts dataset
source("_facts.R")

## Sanity checks
source("_sanity_checks.R")

## Joins
source("_joins.R")

## Maps
source("_maps.R")

##########
# setting dataset

indices <- sample(seq(1, 3), size = nrow(republican), replace = TRUE, prob = c(.6, .2, .2))
train <- republican[indices == 1,]
test <- republican[indices == 2,]
val <- republican[indices == 3,]

# 11 category cols
category_cols <- c(
  "fips", 
  "map_color", 
  "party_won", 
  "state", 
  "state_abbreviation", 
  "county", 
  "candidate", 
  "party", 
  "lead_party", 
  "area_name", 
  "state_facts"
)

non_demographic_cols <- c(
  "total_votes", 
  "votes", 
  "votes_democrat", 
  "votes_republican", 
  "prop_democrat", 
  "prop_republican",
  "frac_votes", 
  "frac_democrat", 
  "frac_republican", 
  "lead_votes"
)

train_continous <- train[,-which(colnames(train) %in% category_cols)]

correlations_table <- rquery.cormat(train_continous, type="flatten", graph=FALSE)$r %>% 
  filter(!(column %in% non_demographic_cols) & !(row %in% non_demographic_cols)) %>% 
  # Only interested in the cols correlated to prop_republican
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

republican_correlation_table <- rquery.cormat(train_continous, type="flatten", graph=FALSE)$r %>% 
  # Only interested in the cols correlated to prop_republican
  filter(column=="prop_republican" | row=="prop_republican") %>% 
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

predictors <- correlation_table[republican_correlation_table$cor_abs > 0.2,]
predictors <- unique(c(as.character(predictors[,1]), as.character(predictors[,2])))

predictors <- predictors[!(predictors %in% non_demographic_cols)]
#correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])

correlations <- cor(facts_vars[,which(colnames(facts_vars) %in% predictors)])
corrplot::corrplot(correlations, method="circle")

#correlations <- cor(train[,-which(colnames(train) %in% c("fips", "map_color", "party_won", "state", "state_abbreviation", "county", "candidate", "party", "lead_party", "area_name", "state_facts"))])

#source("_correlations.R")
cols <- colnames(republican)
cols_facts <- colnames(facts_vars)

for (col in predictors) {
  if (length(which(cols==col))==0) {
    next
  }
  if (length(which(cols_facts==col))==0) {
    next
  }  
  
  plot(density(facts_vars[,which(cols_facts==col)]), main=col)
}


source("_plots.R")


 #####################################

# votes

plot_party_won_boxplot("random_retail_sales_per_capita_2007")

# only the correlated columns are here, for adding more check first the distribution

mapping_columns <- list(
  "population_2014" = "log2(population_2014)",
  "population_density_2010" = "log2(population_density_2010)",
  "population_foreign_percent_2013" = "population_foreign_percent_2013",
  
  "race_asian_percent_2014" = "log2(race_asian_percent_2014 + 1)",
  "race_white_no_hispanic_percent_2014" = "race_white_no_hispanic_percent_2014",
  "race_afroamerican_percent_2014" = "race_afroamerican_percent_2014",
  
  "veterans_percent_2013" = "veterans_percent_2013",
  
  "housing_units_in_multiunit_2013" = "housing_units_in_multiunit_2013",
  "housing_homeownership_rate_2013" = "housing_homeownership_rate_2013",
  "housing_median_value_in_housing_units_2013" = "housing_median_value_in_housing_units_2013",
  
  "age_over_65_percent_2014" = "age_over_65_percent_2014",
  "age_under_18_percent_2014" = "age_under_18_percent_2014",
  "age_under_5_percent_2014" = "age_under_5_percent_2014",
  
  "random_retail_sales_per_capita_2007" = "random_retail_sales_per_capita_2007",
  "income_persons_below_poverty_percent_2013" = "income_persons_below_poverty_percent_2013",
  
  "education_bachelor_percent_2013" = "education_bachelor_percent_2013"
  
  # "businesses_afroamerican_rate_2007" = TODO
)

formula <- ""

for (predictor in predictors) {
  if (!(predictor %in% names(mapping_columns))) {
    warning(paste("missing formula for predictor: ", predictor))
    next
  }
  
  sep <- ifelse(formula=="", " ", " + ")
  formula <- paste(formula, mapping_columns[[predictor]], sep=sep)
}

formula <- paste("party_won_num ~ ", formula, sep="")
formula <- (as.formula(formula))

model <- glm(formula, data=train, family = binomial)
p_values <- coef(summary(model))[,4]


##### Penalized logistic regression
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
x <- model.matrix(formula, train)
y <- train$party_won_num

glmnet(x, y, family = "binomial", alpha = 0, lambda = NULL)

##### Removing predictors

as.data.frame(p_values[order(p_values)])

formula <- ""


library(glmnet)

for (predictor in predictors) {
  if (!(predictor %in% names(mapping_columns))) {
    warning(paste("missing formula for predictor: ", predictor))
    next
  }
  
  predictor <- mapping_columns[[predictor]]
  print("p_value")
  print(p_values[[predictor]])
  #sep <- ifelse(formula=="", " ", " + ")
  #formula <- paste(formula, mapping_columns[[predictor]], sep=sep)
}


# http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R
HLTest(glm.fit, g=6)

pR2(glm.fit)


evaluation <- evaluate_model(model, test, "party_won")


Anova(model, 
      type="II", 
      test="Wald")

nagelkerke(model)
summary(model)

emplogit(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)
empLogitPlot(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)




