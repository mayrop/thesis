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
library(sf)
library(ggplot2)
library(GGally)
library(gridExtra)
library(dplyr)
library(moderndive)
#library(ISLR)
library(skimr)
library(plotly)
library(tidyr)
library(tidyverse)
# install.packages('devtools')
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
#library(datasets)
#library(knitr)
#library(janitor)
#library(infer)
#library(broom)
#library(sjPlot)
#library(stats)
#library(readr)
library(reshape2) # correlation headmap

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

# https://www.nytimes.com/elections/2016/results/president
# https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Arizona

#  - FIPS:4007 (Gila County), candidate other shows candidatevotes=15512, but should be: 1123
#  - FIPS:4009 (Graham County), candidate other shows candidatevotes=8980, but should be: 806
#  - FIPS:4011 (Greenlee County), candidate other shows candidatevotes=2208, but should be: 286
fixes <- rbind(
  c(4007, 1123),
  c(4009, 806),
  c(4011, 286)
)

# Apply fixes
for (i in 1:nrow(fixes)) {
  elections_orig[
    elections_orig$FIPS == fixes[i,1] & 
      !is.na(elections_orig$FIPS) & 
      elections_orig$year == 2016 & 
      elections_orig$candidate == "Other",
    ]$candidatevotes <- fixes[i,2]
}

# Recalculate total votes
elections_orig <- elections_orig %>%
  group_by(year,FIPS) %>%
  mutate(totalvotes = sum(candidatevotes)) %>%
  ungroup()

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
  select(
    fips, 
    state, 
    state_abbreviation, 
    county,
    candidate, 
    party, 
    total_votes, 
    votes, 
    frac_votes, 
    lead_votes,
    lead_party,
    map_color,
    party_won
  )

elections$map_color <- factor(elections$map_color)
elections$party_won <- factor(elections$party_won)

facts <- facts_orig %>%
  mutate(
    fips = str_pad(fips, 5, pad="0"),
    state_facts = state_abbreviation
  ) %>%
  filter(
    state_facts != ""
  ) %>%
  select(
    -state_abbreviation
  )

########################################################################
## Sanity checks

# Complete cases: Elections
sum(complete.cases(elections)) / nlevels(elections$party)

# Complete cases: Facts
sum(complete.cases(facts))

# Number of rows
nrow(elections_uni)
nrow(elections) / nlevels(elections$party)

# Number of counties won by candidate
sum(elections_uni$partywonR)
sum(elections[elections$candidate=="Donald Trump",]$party_won)

# Total of votes per candidate
sum(elections_uni$candidatevotesR)
sum(elections[elections$candidate=="Donald Trump",]$votes)

########################################################################

all <- full_join(
  elections, facts, by="fips"
)

# Missing values

# Counties with missing votes information
all[is.na(all$party) & all$state_facts != "AK",]
# 3 counties

all[is.na(all$area_name) & all$state_abbreviation != "AK",]
# 1 county, Kansas City

# Removing incomplete rows
all <- all[complete.cases(all),]

########################################

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
  by = c("fips" = "county_fips"))

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



########################################
# Map by final election by county
spatial_data %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = EDU685213), color = "#ffffff", size = 0.05)
########################################

# RHI125214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214, RHI725214, RHI825214
# POP715213, POP645213, POP815213
# EDU685213, EDU635213
# SBO001207, SBO315207, SBO115207, SBO215207, SBO515207, SBO415207, SBO015207

elections_ggpairs <- elections %>%
  select(fracvotesR, partywonR, SBO001207, SBO315207, SBO115207, SBO215207, SBO515207, SBO415207, SBO015207) %>%
  mutate(partywonR = as.character(partywonR))

ggpairs(elections_ggpairs, aes(colour = partywonR, alpha = 0.4))
