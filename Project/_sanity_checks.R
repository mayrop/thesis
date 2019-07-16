#nrow(elections_orig[elections_orig$year==2016 & !is.na(elections_orig$state_po), ]) / 3
#elections_fips <- unique(elections_orig[elections_orig$year==2016 & !is.na(elections_orig$state_po), "FIPS"])

# one more because of the one that dissappearded
length(elections$fips)
elections_fips <- as.numeric(elections[elections$state_abbreviation!="AK",]$fips)

length(facts$fips)
facts_fips <- as.numeric(facts[facts$state_facts!="AK",]$fips)

diff <- facts_fips[! facts_fips %in% elections_fips]
facts[facts$fips %in% diff,]

diff <- elections_fips[! elections_fips %in% facts_fips]
elections[elections$fips %in% diff,]

####################
sum(elections$votes_other) / sum(elections$votes_total)
# [1] 0.05620822
sum(elections$votes_republican) / sum(elections$votes_total)
# [1] 0.4614116
sum(elections$votes_democrat) / sum(elections$votes_total)
# [1] 0.4821184
####################

# Complete cases: Elections
sum(complete.cases(elections)) / length(unique(elections$party))

# Complete cases: Facts
sum(complete.cases(facts_vars))
nrow(facts_vars)

# Number of rows
nrow(elections_uni)
nrow(elections) / nlevels(elections$party)

# Number of counties won by candidate
sum(elections_uni$partywonR)
dim(elections[elections$party_won=="republican",])

# 4 counties difference
elections_uni_donald <- as.numeric(elections_uni[elections_uni$partywonR==1,]$FIPS)
elections_other_donald <- as.numeric(elections[elections$party_won=="republican",]$fips)
missing <- elections_other_donald[!(elections_other_donald %in% elections_uni_donald)]

View(elections[as.numeric(elections$fips) %in% missing,])
View(elections_uni[as.numeric(elections_uni$FIPS) %in% missing,])
# 4 missing
# NE missing, reported & fixed
# 3 incorrect from AZ, reported & fixed

# Total of votes per candidate
sum(elections$votes_republican) - sum(elections_uni$candidatevotesR)
# Difference (460) is in NE (the new county that was added)

############################

elections_fips <- unique(as.numeric(elections[elections$state_abbreviation!="AK",]$fips))
facts_fips <- unique(as.numeric(facts[facts$state_facts!="AK",]$fips))

print("Extras in elections")
elections[elections$fips %in% elections_fips[!(elections_fips %in% facts_fips)] ,]

print("Extras in facts")
facts[facts$fips %in% facts_fips[!(facts_fips %in% elections_fips)],]
# 15005 does not have data

############################ 

scraped <- read.csv("scraped/2012-2016.csv")
scraped_orig <- scraped
scraped <- scraped[!is.na(scraped$county_fips),]

dem_won_fips <- scraped[scraped$votes_dem_2016>scraped$votes_gop_2016,]$combined_fips
dem_won_fips_mine <- as.numeric(elections[elections$votes_democrat>elections$votes_republican&elections$state_abbreviation!="AK",]$fips)

dem_won_fips_mine[!(dem_won_fips_mine %in% dem_won_fips)]
dem_won_fips[!(dem_won_fips %in% dem_won_fips_mine)]

check <- c(dem_won_fips_mine[!(dem_won_fips_mine %in% dem_won_fips)], dem_won_fips[!(dem_won_fips %in% dem_won_fips_mine)])
# facts[facts$fips==46113 &  !is.na(facts$fips),]$fips <- 46102

