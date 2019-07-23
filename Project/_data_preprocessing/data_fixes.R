# Changes per:
# https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf

elections <- elections_orig
facts <- facts_orig

#########################################
# Fixing factor for "Other" candidate (since it has <NA>)
elections$party <- as.character(elections$party)

elections[elections$candidate=="Other", "party"] <- "other"
elections$party <- factor(elections$party)
#########################################

#########################################
# Shannon County, SD (FIPS code=46113). Effective May 1, 2015, Shannon County was 
# renamed Oglala Lakota County and given a new FIPS code (46102). 
# It last appeared in the bridged-race Vintage 2014 population files. 
# Shannon County continues to appear on NCHS birth and mortality files. 
#
elections[elections$FIPS==46113 & !is.na(elections$FIPS),]$FIPS <- 46102
facts[facts$fips==46113 &  !is.na(facts$fips),]$fips <- 46102
#
#########################################

#########################################
# Bedford City, Virginia (FIPS code = 51515). Effective July 1, 2013, Bedford city, 
# Virginia (51515), formerly an independent city,  was added to Bedford County (51019)
#
elections[(elections$year==2016 & elections$FIPS==51515 & !is.na(elections$FIPS)),] <- NA
facts[facts$fips==51515 &  !is.na(facts$fips),] <- NA
#
#########################################

#########################################
# Removing incomplete rows (only the one we just removed)
#
facts <- facts[complete.cases(facts),]
#
#########################################

#########################################
# Looks like a few invalid rows exist for "counties" without FIPS
# They are not valid counties, so we filter them out
elections <- elections[!is.na(elections$FIPS),]
#########################################

# Joining Kansas City & Jackson County
cond1 <- elections$FIPS == 29095 & elections$year==2016
cond2 <- elections$FIPS == 36000 & elections$year==2016

e29095 <- elections[cond1, c("party", "candidatevotes")]
e36000 <- elections[cond2, c("party", "candidatevotes")]

elections_fix <- inner_join(e29095, e36000, by="party")
elections_fix$candidatevotes <- elections_fix$candidatevotes.x + elections_fix$candidatevotes.y

for (party in elections[cond1,]$party) {
  cond <- cond1 & elections$party == party
  elections[cond,]$candidatevotes <- elections_fix[elections_fix$party == party,]$candidatevotes
}

elections[cond1,]$totalvotes <- sum(elections[cond1,]$candidatevotes)
elections[cond2,] <- NA

###### Remove this one again
elections <- elections[!is.na(elections$FIPS),]
