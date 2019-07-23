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
source("_settings/load_data.R")

# S E T T I N G S

source("_settings/libraries.R")
source("_settings/functions.R")
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
    
##########
# setting dataset

indices <- sample(seq(1, 2), size = nrow(all), replace = TRUE, prob = c(0.6, 0.4))

train.data <- all[indices == 1,]
test.data <- all[indices == 2,]

source("_eda/correlations.R")
source("_eda/maps.R")

cols <- colnames(republican)
cols_facts <- colnames(facts)

for (col in predictors) {
  if (length(which(cols==col))==0) {
    next
  }
  if (length(which(cols_facts==col))==0) {
    next
  }  
  
  plot(density(facts[,which(cols_facts==col)]), main=col)
}


plot_party_won_boxplot("housing_units_in_multiunit_13") 
plot_party_won_boxplot("race_white_no_hisp_pct_14") 
plot_party_won_boxplot("edu_bachelor_pct_13") 
plot_party_won_boxplot("race_asian_pct_14")
plot_party_won_boxplot("pop_foreign_pct_13")
plot_party_won_boxplot("log(pop_14)")
plot_party_won_boxplot("inc_med_househ_income_13")
plot_party_won_boxplot("housing_median_val_housing_units_13")
plot_party_won_boxplot("pop_other_lang_pct_13")
plot_party_won_boxplot("pop_foreign_pct_13")
plot_party_won_boxplot("log(pop_density_10)")


## Maps
source("_maps.R")

 #####################################

# votes

# only the columns that need transformations

mapping_columns <- list(
  "pop_14" = "log2(pop_14)",
  "pop_density_10" = "log2(pop_density_10)"
)

formula <- ""

for (predictor in predictors) {
  if (!grepl("_13", predictor) & !grepl("_14", predictor)) {
    next
  }
  
  if (!(predictor %in% names(mapping_columns))) {
    mapping_columns[[predictor]] <- predictor
  }
  
  sep <- ifelse(formula == "", " ", " + ")
  formula <- paste(formula, mapping_columns[[predictor]], sep=sep)
}

formula <- as.formula(paste("party_republican_won ~ ", formula, sep=""))

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

formula <- paste("party_republican_won ~ ", formula, sep="")
formula <- (as.formula(formula))
model <- glm(formula, data=train.data, family = binomial)
summary(model)

accuracy(list(model), plotit=TRUE, digits=3)

evaluate <- evaluate_model(model, test.data, "party_republican_won")
RMSPE(y_pred = evaluate$y_predictions + 1, y_true = evaluate$y_true + 1)

plot(evaluate$performance)

HosmerLemeshow(model, g = 10)

HLTest2(model, g=8)

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


