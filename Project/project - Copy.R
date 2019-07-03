###
#
# University of Glasgow
# Assessing the impact of socio-economic factors on Presidential Primary Election voting in the USA in 2016
#
# Datasets:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# 
#
# For year 2016:
#  - FIPS:4007 (Gila County), candidate other shows candidatevotes=15512, but should be: 1123
#  - FIPS:4009 (Graham County), candidate other shows candidatevotes=8980, but should be: 806
#  - FIPS:4011 (Greenlee County), candidate other shows candidatevotes=2208, but should be: 286
#  
#  https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Arizona
#  https://uselectionatlas.org/RESULTS/state.php?year=2016&fips=4&f=0&off=0&elect=0
### 
# https://www.datacamp.com/community/tutorials/logistic-regression-R

# ip <- as.data.frame(installed.packages())
# head(ip)
# ip <- subset(ip, !grepl("MRO", ip$LibPath))


# Loading Libraries
libraries <- c(
  "ggplot2", "GGally", "gridExtra", "dplyr", 
  "moderndive", "skimr", "plotly", "tidyr", "tidyverse", 
  "urbnmapr", "reshape2", "corrplot", "caret",
  "ellipse", # https://stackoverflow.com/questions/44502469/r-featureplot-returning-null
  "psych", "betareg", "emmeans", "lmtest", # https://rcompanion.org/handbook/J_02.html
  "car", "rcompanion", "e1071", "sf"
)

# devtools::install_github("UrbanInstitute/urbnmapr")

for (library in libraries) {
  if (!require(library, character.only = TRUE)) {
    #install.packages(library)
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

# https://www.datacamp.com/community/tutorials/logistic-regression-R
source("http://www.sthda.com/upload/rquery_cormat.r")

# 11 category cols
category_cols <- c("fips", "map_color", "party_won", "state", "state_abbreviation", "county", "candidate", "party", "lead_party", "area_name", "state_facts")
train_continous <- train[,-which(colnames(train) %in% category_cols)]

correlation_table <- rquery.cormat(train_continous, type="flatten", graph=FALSE)$r
# Only interested in the cols correlated to prop_republican
correlation_table <- correlation_table[correlation_table$col=="prop_republican" | correlation_table$row=="prop_republican",]
  
correlation_table$cor_abs <- abs(correlation_table$cor)
correlation_table <- correlation_table[order(-correlation_table$cor_abs),]

corrs <- correlation_table[correlation_table$cor_abs > 0.20,]
corrs_cols <- unique(c(as.character(corrs[,1]),as.character(corrs[,2])))
non_demographic_cols <- c("total_votes", "votes", "votes_democrat", "votes_republican", "prop_democrat", "frac_votes", "frac_democrat", "frac_republican", "lead_votes")

corrs_cols <- corrs_cols[!(corrs_cols %in% non_demographic_cols)]
#correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])
#correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% corrs_cols)])
#corrplot(correlations, method="circle")

#correlations <- cor(train[,-which(colnames(train) %in% c("fips", "map_color", "party_won", "state", "state_abbreviation", "county", "candidate", "party", "lead_party", "area_name", "state_facts"))])

model_cols <- c("housing_units_in_multiunit_2013", "race_white_no_hispanic_percent_2014", "education_bachelor_percent_2013", 
                "race_afroamerican_percent_2014", "housing_median_value_in_housing_units_2013", "population_foreign_percent_2013",
                "businesses_women_rate", "population_2014", "population_density_2010", "age_under_18_percent_2014", "veterans_percent_2013")
correlations <- cor(train[,which(colnames(train) %in% model_cols)])

corrplot(correlations, method="circle")


#source("_correlations.R")
facts_vars %>% 
  select(housing_units_in_multiunit_2013) %>% 
  skim()

cols <- colnames(facts_vars)

for (col in corrs_cols) {
  if (length(which(cols==col))==0) {
    next
  }
  plot(density(facts_vars[,which(cols==col)]), main=col)
}

plot(density(facts_vars$race_white_no_hispanic_percent_2014))
 #####################################

# votes

my_new_0 <- 0.00000000001
log(0.01)
train <- train %>% 
  mutate(
    race_asian_percent_2014=ifelse(race_asian_percent_2014==0, my_new_0, race_asian_percent_2014),
    race_afroamerican_percent_2014=ifelse(race_afroamerican_percent_2014==0, my_new_0, race_afroamerican_percent_2014),
    businesses_asian_rate=ifelse(businesses_asian_rate==0, my_new_0, businesses_asian_rate),
    businesses_afroamerican_rate=ifelse(businesses_afroamerican_rate==0, my_new_0, businesses_afroamerican_rate),
    population_foreign_percent_2013=ifelse(population_foreign_percent_2013==0, my_new_0, population_foreign_percent_2013),
    businesses_hispanic_rate=ifelse(businesses_hispanic_rate==0, my_new_0, businesses_hispanic_rate),
    random_accomodation_and_food_sales_rate_2007=ifelse(random_accomodation_and_food_sales_rate_2007==0, my_new_0, random_accomodation_and_food_sales_rate_2007),
    age_over_65_percent_2014=ifelse(age_over_65_percent_2014==0, my_new_0, age_over_65_percent_2014),
    age_under_18_percent_2014=ifelse(age_under_18_percent_2014==0, my_new_0, age_under_18_percent_2014),
    random_retail_sales_rate_2007=ifelse(random_retail_sales_rate_2007==0, my_new_0, random_retail_sales_rate_2007),
    businesses_hispanic_rate=ifelse(businesses_hispanic_rate==0, my_new_0, businesses_hispanic_rate),
    businesses_rate=ifelse(businesses_rate==0, my_new_0, businesses_rate)
  )

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


glm.probs <- predict(glm.fit, cal, type="response")
glm.probs
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, cal$party_won)
mean(glm.pred==cal$party_won)

misClasificError <- mean(glm.pred !=cal$party_won)
print(paste('Accuracy',1-misClasificError))

library(ROCR)

p <- predict(glm.fit, cal, type="response")
pr <- prediction(p, cal$party_won)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#glm.pred    0    1
#0  306   82
#1  183 2533
#> mean(glm.pred==republican$party_won)
#[1] 0.9146263

glm.fit <- glm(ResponseVotes ~ 
                 (population_2014) +
                 (population_density_2010) +
                # (age_under_5_percent_2014) +
                # (age_under_18_percent_2014) +
                 (age_over_65_percent_2014) +
                 (education_bachelor_percent_2013) + 
                 (race_white_percent_2014) +
                 (race_afroamerican_percent_2014) + 
                 (race_latino_percent_2014) +
                 (race_white_no_hispanic_percent_2014) + 
                 (hbb_housing_units_rate_2014) +
                 (income_per_capita_income_2013) + 
                 (income_persons_below_poverty_2013) + 
                 (veterans_percent_2013), data = republican, family = binomial)

Anova(glm.fit, 
      type="II", 
      test="Wald")

nagelkerke(glm.fit)
summary(glm.fit)

plotPredy(data  = mydata,
          y     = prop_republican,
          x     = education_bachelor_percent_2013,
          model = glm.fit,
          type  = "response",    # Needed for logistic regression
          xlab  = "Grade",
          ylab  = "Proportion passing")


plot(fitted(model.beta),
     residuals(model.beta))






library(caret)
data(iris)
data(GermanCredit)


Train <- createDataPartition(GermanCredit$Class, p=0.6, list=FALSE)
training <- GermanCredit[Train,]
testing <- GermanCredit[-Train,]

mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
                   CreditHistory.Critical,  data=training, method="glm", family="binomial")
exp(coef(mod_fit$finalModel))

print(mod_fit)

predict(mod_fit, newdata=testing)
predict(mod_fit, newdata=testing, type="prob")

mod_fit_one <- glm(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
                     CreditHistory.Critical, data=training, family="binomial")

mod_fit_two <- glm(Class ~ Age + ForeignWorker, data=training, family="binomial")

anova(mod_fit_one, mod_fit_two, test ="Chisq")

library(lmtest)
lrtest(mod_fit_one, mod_fit_two)

library(pscl)
pR2(mod_fit_one)

library(MKmisc)
HLgof.test(fit = fitted(mod_fit_one), obs = training$Class)
HLgof.test(fit = fitted(glm.fit), obs = train$party_won)

library(ResourceSelection)
hoslem.test(train$party_won, fitted(glm.fit), g=10)

# https://www.r-bloggers.com/evaluating-logistic-regression-models/