# file:///Users/mayrop/Downloads/pols_w3245_2009_brown.pdf
# https://www.jstor.org/stable/pdf/2771413.pdf?refreqid=excelsior%3Ae489f6147ca41ef0465307bb41a41004
# https://www.jstor.org/stable/pdf/2111783.pdf?refreqid=excelsior%3A39b9c326b792e9824c086413c72b3252
# https://essay.utwente.nl/73364/1/Drewer_BA_BMS.pdf
# https://scholarworks.wmich.edu/cgi/viewcontent.cgi?referer=https://www.google.com/&httpsredir=1&article=2044&context=dissertations
# https://www.demos.org/research/why-voting-matters-large-disparities-turnout-benefit-donor-class
# file:///Users/mayrop/Downloads/Socioeconomic_Status_and_Nonvoting_A_Cross-Nationa.pdf

library(ggplot2)
library(GGally)
library(gridExtra)
library(dplyr)
library(moderndive)
#library(ISLR)
library(skimr)
library(plotly)
library(tidyr)
#library(datasets)
#library(knitr)
#library(janitor)
#library(infer)
#library(broom)
#library(sjPlot)
#library(stats)
#library(readr)


## dictionary
# PST045214
# PST120214
# POP010210
# AGE135214
# AGE295214
# AGE775214
# SEX255214
# RHI125214
# RHI225214
# RHI325214
# RHI425214
# RHI525214
# RHI625214
# RHI725214
# RHI825214
# POP715213
# POP645213
# POP815213
# EDU635213
# EDU685213
# VET605213
# LFE305213
# HSG010214
# HSG445213
# HSG096213
# HSG495213
# HSD410213
# HSD310213
# INC910213
# INC110213
# PVY020213
# BZA010213
# BZA110213
# BZA115213
# NES010213
# SBO001207
# SBO315207
# SBO115207
# SBO215207
# SBO515207
# SBO415207
# SBO015207
# MAN450207
# WTN220207
# RTN130207
# RTN131207
# AFN120207
# BPS030214
# LND110210
# POP060210 

# Assessing the impact of socio-economic factors on Presidential election voting in the USA in 2016

# Data available
# You are provided a data set on Presidential election results for each US county in 2016
# PresElect2016R.csv and socio-economic data from the US Census Bureau (until 2014), in
# the file UScounty-facts.csv. An additional file, UScounty-dictionary.csv, is provided,
# which lists the detailed descriptions of variables available in the county facts file. For the
# purposes of this analysis, you may assume that there was an election involving only two
# parties in each county: Republican and Democratic. A brief description of the variables in
# the files are listed below:

# File 1: PresElect2016R.csv
# Column 1: state
# Column 2: state.po (2-letter state abbreviation)
# Column 3: county (county name)
# Column 4: FIPS (unique ID for county from US Census records)
# Column 5: candidatevotesR (number of votes cast for Republican presidential candidate)
# Column 6: totalvotes (total number of votes cast in the county)
# Column 7: fracvotesR (fraction of total votes received by the Republican Presidential candidate)
# Column 8: partywonR (binary variable that takes the value 1 if the Republican candidate 
#           won in that county; is otherwise zero)

# File 2: UScounty-facts.csv
# The columns of this file correspond to measurements on several variables for each county,
# described in UScounty-dictionary.csv. Variables 1-18 correspond to demographic variables
# relating to the population and racial composition of counties. Variables 19 and 20 correspond
# to educational attainment; variable 21 to the number of war veterans in the county; variables
# 22-28 relate to housing; variables 29-42 to income and employment; variables 43-47 to sales;
# and variables 48-50 to building permits, land area and population per square mile, respectively.

# Question(s) of interest
# The main questions of interest are twofold: first, are there any discernible associations between
# various socio-economic and other factors and the propensity of the county population to vote
# for a particular party? Second, can the relationship between various factors and primary
# election results by county be consolidated into a model that can forecast the actual 2016
# presidential election results, by state? In particular, you may want to consider:
# - Are there specific socio-economic or demographic factors that are associated with an
#   increased or decreased preference for a political party, in a county?
# - Is there an association between specific socio-economic or demographic factors and the
#   fraction of people voting for a Republican Presidential candidate in a county?
# - Are there state-wide factors that are associated with a preference for one political party
#   over another?
# - How well can your model associating socioeconomic factors with 2016 election results
#   be used to predict the final state-wide outcome of the presidential elections in 2016?
# (For this question you might want to locate a data set listing the winning party in each
# state- this is available on numerous internet news sites, such as CNN.com or NPR.org;
# alternatively, you can consolidate data from within your existing data set.)

###########

#  The problem, to start with, is manipulating data sets with large numbers of variables 
# and merging records across diferent data sets (electoral outcomes and socio-economic variables). 
# It will be useful to keep in mind that certain counties listed in one file may or 
# may not appear in the other; also there may be counties in which either electoral or census 
# data may have to be discarded for some reason, for example, missingness in some variables. 
# Groups of demographic/economic variables may be related by nature and must be handled carefully
# (e.g. you may not be able to use them simultaneously in a regression-type model). 
# Some variables measured are on widely diferent scales of magnitude from others and care must be 
# taken to ensure that any analysis undertaken is not afected by this. 
# Preliminary exploratory analyses would be invaluable in determining which variables to look at 
# more closely for the later statistical analysis. 
# Note that you might want to use either the data on winning party,
# or proportion of votes won, as a response - they may not give identical results. 

# Use of the maps package might be useful for visualisation of the distribution of variables 
# across counties/states. For the advanced chapter, one option would be to source data from the 
# earlier election (see, for example, US Presidential election data, 2012, from 
# https://electionlab.mit.edu/data) and corresponding census data (http://www.census.gov) 
# and determine whether socio-economic changes over the intervening period can 
# explain the diferences in election results between the two years.

# Reading material
# 1. Presidential Election Process. https://www.usa.gov/election
# 2. A Comprehensive Survey of Clustering Algorithms. Xu, D. and Tian, Y. (2015) 
# Annals of Data Science 2 (2), 165–193. https://link.springer.com/article/10.1007/ s40745-015-0040-1
# 3. Supervised machine learning: A review of classification techniques. 
# Kotsiantis, S. B. (2007) Informatica, 31, 249-268. 
# http://www.informatica.si/ojs-2.4.3/index.php/ informatica/article/view/148/140
setwd("~/Github/thesis/Project")

elections_orig <- read.csv("data/PresElect2016R.csv")
dictionary <- read.csv("data/UScounty-dictionary.csv")
facts <- read.csv("data/UScounty-facts.csv")

head(elections_orig)
head(dictionary)
head(facts)

elections <- elections_orig %>%
  mutate(fips = FIPS) %>%
  select(fips, state, state.po, candidatevotesR, totalvotes, fracvotesR, partywonR)

# joining tables
elections <- right_join(elections, facts, by=c("fips"))
# backup of rows without data
missing_elections <- elections[is.na(elections$state),]

elections <- elections[!is.na(elections$state),]
elections <- elections %>%
  select(-state, -state_abbreviation)

# skim for checking missing values
elections_orig %>% skim()
facts %>% 
  select(-area_name, -state_abbreviation) %>% 
  skim()

# RHI125214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214, RHI725214, RHI825214
# POP715213, POP645213, POP815213
# EDU685213, EDU635213
# SBO001207, SBO315207, SBO115207, SBO215207, SBO515207, SBO415207, SBO015207

elections_ggpairs <- elections %>%
  select(fracvotesR, partywonR, SBO001207, SBO315207, SBO115207, SBO215207, SBO515207, SBO415207, SBO015207) %>%
  mutate(partywonR = as.character(partywonR))

ggpairs(elections_ggpairs, aes(colour = partywonR, alpha = 0.4))
