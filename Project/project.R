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
  "frac_votes", 
  "frac_democrat", 
  "frac_republican", 
  "lead_votes"
)

train_continous <- train[,-which(colnames(train) %in% category_cols)]

correlation_table <- rquery.cormat(train_continous, type="flatten", graph=FALSE)$r %>% 
  # Only interested in the cols correlated to prop_republican
  filter(column=="prop_republican" | row=="prop_republican") %>% 
  mutate(cor_abs = abs(cor)) %>% 
  arrange(desc(cor_abs))

correlated_columns <- correlation_table[correlation_table$cor_abs > 0.20,]
correlated_columns <- unique(c(as.character(correlated_columns[,1]), as.character(correlated_columns[,2])))

correlated_columns <- correlated_columns[!(correlated_columns %in% non_demographic_cols)]
#correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])

correlations <- cor(facts_vars[,which(colnames(facts_vars) %in% correlated_columns)])
corrplot(correlations, method="circle")

#correlations <- cor(train[,-which(colnames(train) %in% c("fips", "map_color", "party_won", "state", "state_abbreviation", "county", "candidate", "party", "lead_party", "area_name", "state_facts"))])

#source("_correlations.R")
cols <- colnames(republican)
cols_facts <- colnames(facts_vars)

for (col in correlated_columns) {
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


glm.fit.complete <- glm(party_won ~ 
                 race_white_no_hispanic_percent_2014 +
                 log(age_over_65_percent_2014) +
                 housing_units_in_multiunit_2013 +
                 housing_homeownership_rate_2013 +
                 education_bachelor_percent_2013 +           
                 housing_median_value_in_housing_units_2013 +
                 log(race_asian_percent_2014 + 1) +
                 race_afroamerican_percent_2014 +             
                 population_foreign_percent_2013 +
                 log(population_2014) +
                 log(population_density_2010) +                    
                 veterans_percent_2013 +
                 income_per_capita_income_past_12_month_2013 +
                 random_retail_sales_per_capita_2007 +
                 businesses_afroamerican_rate +
                 race_latino_percent_2014, data = train, family = binomial)


glm.fit <- glm(party_won ~ 
                 race_white_no_hispanic_percent_2014 +
                 log(age_over_65_percent_2014) +
                # housing_units_in_multiunit_2013 +
                 housing_homeownership_rate_2013 +
                 education_bachelor_percent_2013 +           
               #  housing_median_value_in_housing_units_2013 +
                 log(race_asian_percent_2014 + 1) +
                 #race_afroamerican_percent_2014 +             
                 population_foreign_percent_2013 +
                 log(population_2014) +
                 #log(population_density_2010) +                    
                 veterans_percent_2013
                 #income_per_capita_income_past_12_month_2013
                 #random_retail_sales_per_capita_2007 +
                 #businesses_afroamerican_rate +
                 #race_latino_percent_2014
               , data = train, family = binomial)

glm.fit2 <- glm(party_won ~ 
                 race_white_no_hispanic_percent_2014 +
                 log(age_over_65_percent_2014) +
                 housing_homeownership_rate_2013 +
                 education_bachelor_percent_2013 +           
                 housing_median_value_in_housing_units_2013 +
                 log(race_asian_percent_2014 + 1) +
                 #race_afroamerican_percent_2014 +             
                 population_foreign_percent_2013 +
                 log(population_2014) +
                 #log(population_density_2010) +                    
                 veterans_percent_2013 +
                 income_per_capita_income_past_12_month_2013
               #random_retail_sales_per_capita_2007 +
               #businesses_afroamerican_rate +
               #race_latino_percent_2014
               , data = train, family = binomial)

glm.fit3 <- glm(party_won ~ 
                  race_white_no_hispanic_percent_2014 +
                  log(age_over_65_percent_2014) +
                  housing_units_in_multiunit_2013 +
                  education_bachelor_percent_2013 +           
                  housing_median_value_in_housing_units_2013 +
                  log(race_asian_percent_2014 + 1) +
                  #race_afroamerican_percent_2014 +             
                  population_foreign_percent_2013 +
                  log(population_2014) +
                  #log(population_density_2010) +                    
                  veterans_percent_2013 +
                  income_per_capita_income_past_12_month_2013
                #random_retail_sales_per_capita_2007 +
                #businesses_afroamerican_rate +
                #race_latino_percent_2014
                , data = train, family = binomial)


glm.fit <- glm(party_won ~ 
                # housing_units_in_multiunit_2013 +
                 race_white_no_hispanic_percent_2014 +
                 education_bachelor_percent_2013 +
                # log(race_afroamerican_percent_2014) +
               #  log(housing_median_value_in_housing_units_2013) +
                # log(race_asian_percent_2014) +
                 #log(businesses_asian_rate) +
                # log(businesses_afroamerican_rate) +
                 log(population_foreign_percent_2013) +
                 businesses_women_rate +
                 log(population_2014) +
                 #log(businesses_rate) +
                 #log(businesses_hispanic_rate) +
                # log(random_accomodation_and_food_sales_rate_2007) +
                 log(population_density_2010) +
                # income_per_capita_income_past_12_month_2013 +
                 #log(age_over_65_percent_2014) +
                 log(age_under_18_percent_2014) 
                # log(age_under_5_percent_2014) +
                # log(random_retail_sales_rate_2007) +
                # housing_homeownership_rate_2013 +
               #  veterans_percent_2013
               , data = train, family = binomial)


# http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R
HLTest(glm.fit, g=8)

pR2(glm.fit)

glm.probs <- predict(glm.fit, type="response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

table(glm.pred, train$party_won)
mean(glm.pred==train$party_won)


mydata <- train %>% ungroup() %>%
  dplyr::select(
    housing_units_in_multiunit_2013,
    race_white_no_hispanic_percent_2014,
    education_bachelor_percent_2013,
    race_afroamerican_percent_2014,
    housing_median_value_in_housing_units_2013,
    race_asian_percent_2014,
    businesses_asian_rate,
    businesses_afroamerican_rate,
    population_foreign_percent_2013,
    businesses_women_rate,
    population_2014,
    businesses_rate,
    businesses_hispanic_rate,
    random_accomodation_and_food_sales_rate_2007,
    population_density_2010,
    income_per_capita_income_past_12_month_2013,
    age_over_65_percent_2014,
    age_under_18_percent_2014,
    age_under_5_percent_2014,
    random_retail_sales_rate_2007,
    housing_homeownership_rate_2013,
    veterans_percent_2013
  )

predictors <- colnames(mydata)


mydata$logit <- log(glm.probs/(1-glm.probs))

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


p <- predict(glm.fit3, test, type="response")
pr <- prediction(p, test$party_won)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

write.csv(republican, file="republican.csv")

#glm.pred    0    1
#0  306   82
#1  183 2533
#> mean(glm.pred==republican$party_won)
#[1] 0.9146263

Anova(glm.fit, 
      type="II", 
      test="Wald")

nagelkerke(glm.fit)
summary(glm.fit)

emplogit(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)
empLogitPlot(log(republican$education_bachelor_percent_2013), as.numeric(republican$party_won)-1)




