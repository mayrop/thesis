####################
sum(elections$votes_other) / sum(elections$votes_total)
# [1] 0.05620822
sum(elections$votes_republican) / sum(elections$votes_total)
# [1] 0.4614116
sum(elections$votes_democrat) / sum(elections$votes_total)
# [1] 0.4823802
####################

# Complete cases: Elections
sum(complete.cases(elections))

# Complete cases: Facts
sum(complete.cases(facts))
nrow(facts)

############################

elections_fips <- unique(as.numeric(elections[elections$state_abbreviation!="AK",]$fips))
facts_fips <- unique(as.numeric(facts[facts$state_facts!="AK",]$fips))

print("Extras in elections")
elections[elections$fips %in% elections_fips[!(elections_fips %in% facts_fips)] ,]

print("Extras in facts")
facts[facts$fips %in% facts_fips[!(facts_fips %in% elections_fips)],]


#################################################################################
######### Cleanup
rm(facts_fips)
rm(elections_fips)

