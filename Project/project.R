###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
### 


# Loading Libraries
libraries <- c(
  "ggplot2", "GGally", "gridExtra", "dplyr", 
  "moderndive", "skimr", "plotly", "tidyr", "tidyverse", 
  "urbnmapr", "reshape2", "corrplot", "caret",
  "ellipse", # https://stackoverflow.com/questions/44502469/r-featureplot-returning-null
  "psych", "betareg", "emmeans", "lmtest", # https://rcompanion.org/handbook/J_02.html
  "car", "rcompanion", "e1071", "sf", "ROCR", "glmnet"
)

libraries <- c(
  "dplyr", # for %>%
  "stringr", # for str_pad
  "skimr",
  "tidyr",
  "ggplot2",
  "ROCR",
  "glmnet" # regularized logistic regression
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


##########
# setting dataset

indices <- sample(seq(1, 2), size = nrow(republican), replace = TRUE, prob = c(0.6, 0.4))

train.data <- republican[indices == 1,]
test.data <- republican[indices == 2,]

# 11 category cols
category_cols <- c(
  "fips", 
  "map_color", 
  "party_won", 
  "party_won_num",
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

train_continous <- train.data[,-which(colnames(train.data) %in% category_cols)]

continous <- republican[,-which(colnames(republican) %in% category_cols)]
continous <- continous[,-which(names(continous) %in% non_demographic_cols)]

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


predictors <- republican_correlation_table[republican_correlation_table$cor_abs > 0.2,]
predictors <- unique(c(as.character(predictors[,1]), as.character(predictors[,2])))

predictors <- predictors[!(predictors %in% non_demographic_cols)]
#correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])

correlations <- cor(facts_vars[,which(colnames(facts_vars) %in% predictors)])
corrplot::corrplot(correlations, method="circle")

### Cluster of variables

correlation <- cor(continous, use="complete.obs", method="pearson")
melted_correlation <- melt(correlation)
correlation[correlation < 0.5] = 0

dissimilarity = 1 - correlation

distance = as.dist(dissimilarity)
cluster = hclust(distance)
plot(cluster, cex=0.6)


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


plot_party_won_boxplot("housing_units_in_multiunit_2013") 
plot_party_won_boxplot("race_white_no_hispanic_percent_2014") 
plot_party_won_boxplot("housing_median_value_in_housing_units_2013") 
plot_party_won_boxplot("education_bachelor_percent_2013") 
plot_party_won_boxplot("race_asian_percent_2014")
plot_party_won_boxplot("population_foreign_percent_2013")
plot_party_won_boxplot("population_2014")
plot_party_won_boxplot("other_accomodation_and_food_sales_rate_2007")
plot_party_won_boxplot("age_under_18_percent_2014")
plot_party_won_boxplot("age_under_5_percent_2014")
plot_party_won_boxplot("age_over_65_percent_2014")
plot_party_won_boxplot("population_density_2010")


source("_plots.R")

## Maps
source("_maps.R")

 #####################################

# votes

# only the correlated columns are here, for adding more check first the distribution

mapping_columns <- list(
  "population_2014" = "log2(population_2014)",
  "population_density_2010" = "log2(population_density_2010)",
  "population_foreign_percent_2013" = "population_foreign_percent_2013",
  
  "race_asian_percent_2014" = "race_asian_percent_2014",
  "race_white_no_hispanic_percent_2014" = "race_white_no_hispanic_percent_2014",
  "race_afroamerican_percent_2014" = "race_afroamerican_percent_2014",
  "race_two_races_percent_2014" = "race_two_races_percent_2014",
  
  "veterans_percent_2013" = "veterans_percent_2013",
  
  "housing_units_in_multiunit_2013" = "housing_units_in_multiunit_2013",
  "housing_homeownership_rate_2013" = "housing_homeownership_rate_2013",
  "housing_median_value_in_housing_units_2013" = "housing_median_value_in_housing_units_2013",
  "housing_households_rate_2013" = "housing_households_rate_2013",
  
  "age_over_65_percent_2014" = "age_over_65_percent_2014",
  "age_under_18_percent_2014" = "age_under_18_percent_2014",
  "age_under_5_percent_2014" = "age_under_5_percent_2014",
  
  "income_persons_below_poverty_percent_2013" = "income_persons_below_poverty_percent_2013",
  "income_per_capita_income_past_12_month_2013" = "income_per_capita_income_past_12_month_2013",
  
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

model <- glm(formula, data=train.data, family = binomial)
summary(model)

all_p_values <- coef(summary(model))[,4]
p_values <- all_p_values[all_p_values < 0.05]

formula <- ""

for (i in 1:length(names(p_values))) {
  predictor <- names(p_values)[i]
  if (predictor == "(Intercept)") {
    next
  }
  
  sep <- ifelse(formula=="", " ", " + ")
  formula <- paste(formula, predictor, sep=sep)
}

formula <- paste("party_won_num ~ ", formula, sep="")
formula <- (as.formula(formula))
model <- glm(formula, data=train.data, family = binomial)


accuracy(list(model), plotit=TRUE, digits=3)

evaluate <- evaluate_model(model3, test.data, "party_won_num")
RMSPE(y_pred = evaluate$y_predictions + 1, y_true = evaluate$y_true + 1)


model3 <- glm("party_won_num ~population_foreign_percent_2013 + housing_units_in_multiunit_2013 
              + race_white_no_hispanic_percent_2014 + education_bachelor_percent_2013 + veterans_percent_2013  ", data=train.data, family = binomial(link="cauchit"))

plot(evaluate_model(model, test.data, "party_won_num")$performance)

##### Penalized logistic regression

x <- model.matrix(formula, train)
newx <- model.matrix(formula, test)

y <- train$party_won_num

# alpha = 1 -> lasso, 0 -> ridge
cv.elasticnet <- cv.glmnet(x, y, alpha = 0.5, family = "binomial",  type.measure = "deviance")

plot(cv.elasticnet)
coef(cv.elasticnet, s = "lambda.min")




predict(cv.elasticnet, newx=newx, s = "lambda.min", type = "class")



HLTest(model.ridge, g=6)

pR2(glm.fit)


evaluation <- evaluate_model(model, test, "party_won")


Anova(model, 
      type="II", 
      test="Wald")

nagelkerke(model)
summary(model)

emplogit(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)
empLogitPlot(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)

ggplot(data = melted_correlation, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


