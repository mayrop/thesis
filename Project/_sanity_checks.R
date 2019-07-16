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
sum(as.numeric(elections[elections$candidate=="Donald Trump",]$party_won) - 1)

# 3 counties difference
elections_uni_donald <- elections_uni[elections_uni$partywonR==1,]$FIPS
elections_other_donald <- as.numeric(elections[elections$candidate=="Donald Trump" & elections$party_won==1,]$fips)
elections_other_donald[!(elections_other_donald %in% elections_uni_donald)]
# these are 3 counties i wrote the owner of the dataset to fix
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ&version=1.0

# Total of votes per candidate
sum(elections_uni$candidatevotesR)
sum(elections[elections$candidate=="Donald Trump",]$votes)

elections_fips <- unique(as.numeric(elections[elections$state_abbreviation!="AK",]$fips))
facts_fips <- unique(as.numeric(facts_vars[facts_vars$state_facts!="AK",]$fips))

diff <- elections_fips[!(elections_fips %in% facts_fips)]
diff2 <- facts_fips[!(facts_fips %in% elections_fips)]

# 31103 -> sent email
# 15005 does not have data