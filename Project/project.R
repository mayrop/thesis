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

# Loading Libraries
libraries <- c(
  "sf", "ggplot2", "GGally", "gridExtra", "dplyr", "moderndive", "skimr", "plotly", "tidyr", "tidyverse", "urbnmapr", "reshape2", "corrplot", "caret",
  "ellipse", # https://stackoverflow.com/questions/44502469/r-featureplot-returning-null
  "e1071",
  "psych", "betareg", "emmeans", "lmtest", # https://rcompanion.org/handbook/J_02.html
  "car", "rcompanion"
)


for (library in libraries) {
  if (!require(library, character.only = TRUE)) {
    install.packages(library)
  }

  library(library, character.only = TRUE)
}

# Setting project directory
setwd("~/Github/thesis/Project")

# Reading datasets
elections_uni <- read.csv("data/counties-uni.csv")
elections_orig <- read.csv("data/counties-original.csv")
dictionary_orig <- read.csv("data/dictionary.csv")
facts_orig <- read.csv("data/facts.csv")


# Changes per:
# https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
elections_orig[elections_orig$FIPS==46113 & !is.na(elections_orig$FIPS),]$FIPS <- 46102
facts_orig[facts_orig$fips==46113 &  !is.na(facts_orig$fips),]$fips <- 46102

########################################################################

testing <- function(lead_party, lead_votes) {
  if (lead_votes <= 0.1) {
    val <- 1
  } else if (lead_votes <= 0.2) {
    val <- 2
  } else if (lead_votes <= 0.3) {
    val <- 3
  } else {
    val <- 4  
  }
  
  if (lead_party == "democrat") {
    val <- val + 4
  }
  
  if (lead_party %in% c("democrat", "republican")) {
    return(val)
  }

  stop("error happened")
}


# Adding variables that exist in the Uni dataset
# Removing NA from party column
elections <- elections_orig
elections$party <- as.character(elections$party)
elections[is.na(elections$party), "party"] <- "other"
elections$party <- factor(elections$party)

elections <- elections %>%
  filter(year == 2016 & !is.na(totalvotes)) %>%
  select(-year, -version, -office) %>%
  mutate(
    frac_votes=candidatevotes / totalvotes,
    fips=str_pad(FIPS, 5, pad="0"),
    state_abbreviation=state_po,
    total_votes=totalvotes,
    votes=candidatevotes
  ) %>% 
  group_by(fips) %>%
  mutate(
    party_won=as.numeric(row_number()==which.max(frac_votes))
  ) %>%
  arrange(fips, desc(frac_votes)) %>%
  mutate(
    lead_votes=frac_votes[1]-frac_votes[2],
    lead_party=party[1],
    map_color=testing(party[1], frac_votes[1]-frac_votes[2])
  ) %>%
  arrange(fips, party) %>%
  mutate(
    frac_democrat=frac_votes[1],
    frac_republican=frac_votes[3],
    votes_democrat=votes[1],
    votes_republican=votes[3],
    prop_democrat=votes[1]/(votes[1] + votes[3]),
    prop_republican=votes[3]/(votes[1] + votes[3])
  ) %>%
  select(
    fips, 
    state, 
    state_abbreviation, 
    county,
    candidate, 
    party, 
    total_votes, 
    votes, 
    votes_democrat,
    votes_republican,
    prop_democrat,
    prop_republican,
    frac_votes,
    frac_democrat,
    frac_republican,
    lead_votes,
    lead_party,
    map_color,
    party_won
  )

elections$map_color <- factor(elections$map_color)
elections$party_won <- factor(elections$party_won)

# Working with the facts

facts_filtered <- facts_orig %>%
  mutate(
    fips = str_pad(fips, 5, pad="0"),
    state_facts = state_abbreviation
  ) %>%
  filter(
    state_facts != ""
  ) %>%
  select(
    -state_abbreviation,
    -RHI325214, -RHI425214, -RHI525214, -RHI625214, # percent of indian, asian, hawaiian and two or more races
    -SBO115207, -SBO215207, -SBO315207, -SBO415207, -SBO515207, -SBO015207, # number of firms by race
    -HSD310213, -HSG096213, -HSG445213, -HSD410213, -HSG495213, # housing
    -INC110213, # household income
    -POP010210, # population 2010
    -BZA110213, -BZA115213, # private nonfarm employment
    -RTN130207, -RTN131207, # retail sales 2007
    -MAN450207, # manufacturers shipments 2007 
    -WTN220207, # merchangt wholesaler sales 2007
    -AFN120207, # accomodation and food services
    -LND110210, # land area in square miles
    -PST120214, # population percent change
    -POP715213, -POP645213, -POP815213, # population related to foreign born persons and language other than english
    -LFE305213 # mean travel time
  )

# Double checking
facts_filtered %>% 
  select(-fips, -area_name, -state_facts) %>% 
  skim()

# Renaming variables for better readibility
facts_vars <- facts_filtered %>%
  mutate(
    # population
    population_2014 = PST045214,
    population_density_2010 = POP060210,
    
    # age
    age_under_5_percent_2014 = (AGE135214 * 100) / PST045214,
    age_under_18_percent_2014 = (AGE295214 * 100) / PST045214,
    age_over_65_percent_2014 = (AGE775214 * 100) / PST045214,
    
    # education
    education_high_school_percent_2013 = EDU635213,
    education_bachelor_percent_2013 = EDU685213,
    
    # race
    race_white_percent_2014 = RHI125214, 
    race_afroamerican_percent_2014 = RHI225214,
    race_latino_percent_2014 = RHI725214,
    race_white_no_hispanic_percent_2014 = RHI825214,
    
    # sex
    females_percentage_2014 = (SEX255214 * 100) / PST045214,
    
    # housing & buildings & businesses
    hbb_building_permits_rate_2014 = BPS030214 / PST045214,
    hbb_private_nonfarm_establishments_rate_2014 = ifelse(PST045214 == 0, 0, BZA010213 / PST045214),
    hbb_housing_units_rate_2014 = HSG010214 / PST045214,
    hbb_nonemployer_establishments_rate_2013 = ifelse(PST045214 == 0, 0, NES010213 / PST045214),
    hbb_businesses_rate_2007 = ifelse(PST045214 == 0, 0, SBO001207 / PST045214),
    
    # money
    income_per_capita_income_2013 = INC910213,
    income_persons_below_poverty_percent_2013 = PVY020213,
    
    # veterans
    veterans_percent_2013 = ifelse(PST045214 == 0, 0, (VET605213 * 100) / PST045214)
  ) %>%
  dplyr::select(
    # ids 
    fips, 
    area_name, 
    state_facts,
    
    # population
    population_2014,
    population_density_2010,
    
    # age
    age_under_5_percent_2014,
    age_under_18_percent_2014,
    age_over_65_percent_2014,
    
    # education
    education_high_school_percent_2013,
    education_bachelor_percent_2013,
    
    # race
    race_white_percent_2014, 
    race_afroamerican_percent_2014,
    race_latino_percent_2014,
    race_white_no_hispanic_percent_2014,
    
    # sex
    # females_percentage_2014,
    
    # housing & buildings & businesses
    hbb_building_permits_rate_2014,
    hbb_private_nonfarm_establishments_rate_2014,
    hbb_housing_units_rate_2014,
    hbb_nonemployer_establishments_rate_2013,
    hbb_businesses_rate_2007,
    
    # money
    income_per_capita_income_2013,
    income_persons_below_poverty_percent_2013,
    
    # veterans
    veterans_percent_2013
  )

# Analysis
facts_vars %>% 
  select(-fips, -area_name, -state_facts) %>% 
  skim()

########################################################################
## Sanity checks

# Complete cases: Elections
sum(complete.cases(elections)) / nlevels(elections$party)

# Complete cases: Facts
sum(complete.cases(facts_vars))
nrow(facts_vars) # TODO - fix
facts_vars <- facts_vars[complete.cases(facts_vars),]

# Number of rows
nrow(elections_uni)
nrow(elections) / nlevels(elections$party)

# Number of counties won by candidate
sum(elections_uni$partywonR)
sum(as.numeric(elections[elections$candidate=="Donald Trump",]$party_won)) # CHECK!

# Total of votes per candidate
sum(elections_uni$candidatevotesR)
sum(elections[elections$candidate=="Donald Trump",]$votes)

#if (ncol(facts_vars) != ncol(facts_filtered)) {
#  stop("You're missing variables")
#}

sum(complete.cases(facts_vars))

########################################################################

all <- full_join(
  elections, facts_vars, by="fips"
)

# Missing values

# Counties with missing votes information
all[is.na(all$party) & all$state_facts != "AK",]
# 3 counties

all[is.na(all$area_name) & all$state_abbreviation != "AK",]
# 1 county, Kansas City

# Removing incomplete rows
all <- all[complete.cases(all),]
republican <- all[all$party=="republican",]

#################################3

# https://www.datacamp.com/community/tutorials/logistic-regression-R
correlations <- cor(facts_vars[,-which(colnames(facts_vars) %in% c("fips", "area_name", "state_facts"))])
corrplot::corrplot(correlations, method="circle")

correlations <- cor(all[,-which(colnames(all) %in% c("fips", "map_color", "party_won", "state", "state_abbreviation", "county", "candidate", "party", "lead_party", "area_name", "state_facts"))])
corrplot::corrplot(correlations, method="pie")

pairs_matrix <- all %>%
  filter(
    party == "republican"
  ) %>%
  ungroup() %>%
  dplyr::select(
    -fips, -state, -state_abbreviation, -county, -candidate, -party, -total_votes, -votes, -lead_votes, -lead_party,
    -map_color, -area_name, -state_facts
  )

pairs(pairs_matrix[,c(1,c(3:7))], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(8:12)], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(13:17)], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(18:23)], col=pairs_matrix$party_won)

x <- pairs_matrix[,-c(2,8)]
y <- pairs_matrix[,]$party_won

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=log(x), y, plot="density", scales=scales)

featurePlot(x=log(x[c(1,c(3:7))]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(8:12)]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(13:18)]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(19:21)]), y=y, plot="pairs", scales=scales)

featurePlot(x=log(x[c(1:7)]), y=y, plot="density", scales=scales)
featurePlot(x=log(x[c(8:12)]), y=y, plot="density", scales=scales)
featurePlot(x=log(x[c(13:18)]), y=y, plot="density", scales=scales)
featurePlot(x=log(x[c(19:21)]), y=y, plot="density", scales=scales)



glm.fit <- glm(party_won ~ 
                 log(population_2014) +
                 log(population_density_2010) +
                 #log(age_under_5_percent_2014) +
                 log(age_under_18_percent_2014) +
                 log(age_over_65_percent_2014) +
                 log(education_bachelor_percent_2013) + 
                 log(race_white_percent_2014) +
                 log(race_afroamerican_percent_2014 + 1) + 
                 log(race_latino_percent_2014) +
                 log(race_white_no_hispanic_percent_2014) + 
                 log(hbb_housing_units_rate_2014) +
                 log(income_per_capita_income_2013) + 
                 log(income_persons_below_poverty_2013) + 
                 log(veterans_percent_2013), data = all[all$party=="republican",], family = binomial)
summary(glm.fit)
#facts_corr <- facts_orig %>%
#  select(-fips, -area_name, -state_abbreviation)

#correlation <- cor(facts_corr, use="complete.obs", method="pearson")
#melted_correlation <- melt(correlation)

#ggplot(data = melted_correlation, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile() + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### hclust
#correlation[correlation < 0.5] = 0
#dissimilarity = 1 - correlation
#distance = as.dist(dissimilarity)
#cluster = hclust(distance)

#plot(cluster, cex=0.6)

########################################
# Map by final election by county
spatial_data <- inner_join(all,
  get_urbn_map(map = "counties", sf = TRUE),
  by = c("fips" = "county_fips")
)

# counties <- get_urbn_map(map = "counties", sf = TRUE)
# missing<- counties[!(counties$county_fips %in% all$fips),]

# Graham County
# Gila County
# Greenly County

spatial_data %>% 
  filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = map_color)) + 
  scale_fill_manual(values = c("#db8f7f", "#cf6a55", "#c4462d", "#c32b0d","#C0CCDD", "#819ABB", "#3D6C99", "#0e4375"))

spatial_data %>% 
  filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = party_won)) + 
  scale_fill_manual(values = c("#c32b0d","#0e4375"))

########################################
# Map by final election by county
spatial_data %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = education_high_school_percent_2013), color = "#ffffff", size = 0.05)
########################################


ggpairs(elections_ggpairs, aes(colour = partywonR, alpha = 0.4))




# votes
ResponseVotes = as.matrix(cbind(all[all$party=="republican", "votes_republican"], all[all$party=="republican", c("votes_democrat")]))

mydata <- all[all$party == "republican", c("education_bachelor_percent_2013", "prop_republican")]
mydata$log_education_bachelor_percent_2013 <- log(mydata$education_bachelor_percent_2013)

model.log = glm(ResponseVotes ~ log_education_bachelor_percent_2013,
                data = mydata,
                family = binomial(link="logit"))

republican <- all[all$party == "republican", ]
republican$log_race_white_percent_2014 <- log(republican$race_white_percent_2014)
republican$log_population_2014 <- log(republican$population_2014)
republican$log_population_density_2010 <- log(republican$population_density_2010)
republican$log_age_under_18_percent_2014 <- log(republican$age_under_18_percent_2014)
republican$log_education_bachelor_percent_2013 <- log(republican$education_bachelor_percent_2013)
republican$log_race_afroamerican_percent_2014 <- log(republican$race_afroamerican_percent_2014) + 1
republican$log_race_latino_percent_2014 <- log(republican$race_latino_percent_2014)
republican$log_hbb_housing_units_rate_2014 <- log(republican$hbb_housing_units_rate_2014)
republican$log_veterans_percent_2013 <- log(republican$veterans_percent_2013)

glm.fit <- glm(party_won ~ 
                 log_population_2014 +
                 log_population_density_2010 +
                 #log_age_under_5_percent_2014 +
                 log_age_under_18_percent_2014 +
                 #log_age_over_65_percent_2014 +
                 log_education_bachelor_percent_2013 + 
                 log_race_white_percent_2014 +
                 #log_race_afroamerican_percent_2014 + 
                 log_race_latino_percent_2014 +
                 #log_race_white_no_hispanic_percent_2014 + 
                 log_hbb_housing_units_rate_2014 +
                 #log_income_per_capita_income_2013 + 
                 #log_income_persons_below_poverty_2013 + 
                 log_veterans_percent_2013, data = republican, family = binomial(link="logit"))



plotPredy(data  = republican,
          y     = prop_republican,
          x     = income_persons_below_poverty_2013,
          model = glm.fit,
          type  = "response")

Anova(glm.fit, 
      type="II", 
      test="Wald")

nagelkerke(glm.fit)



