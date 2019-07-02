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