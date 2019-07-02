# https://www.nytimes.com/elections/2016/results/president
# https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Arizona

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
    map_color=get_map_color(party[1], frac_votes[1]-frac_votes[2])
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
