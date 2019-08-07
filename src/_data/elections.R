###
#
# In this file we process all the variables that will 
# be needed with regards to the elecitons dataset
#
# @author Mayra Valdes @mayrop
# 
### 

elections <- elections %>%
  dplyr::filter(year == 2016) %>%
  dplyr::select(-year, -version, -office) %>%
  mutate(
    frac_votes = candidatevotes / totalvotes,
    fips = str_pad(FIPS, 5, pad="0"),
    state_abbreviation = state_po,
    votes_total = totalvotes,
    votes = candidatevotes
  ) %>% 
  group_by(fips) %>%
  arrange(fips, desc(frac_votes)) %>%
  mutate(
    party_frac_lead = frac_votes[1] - frac_votes[2],
    party_won = party[1],

    response_binary = ifelse(party[1] == "republican", 1, 0),
    response_factor = ifelse(party[1] == "republican", "yes", "no")
  ) %>%
  arrange(fips, party) %>%
  mutate(
    frac_democrat = frac_votes[1],
    frac_other = frac_votes[2],
    frac_republican = frac_votes[3],
    
    votes_democrat = votes[1],
    votes_other = votes[2],
    votes_republican = votes[3],
    
    prop_democrat = votes[1] / (votes[1] + votes[3]), # renormalizing after removing "other"
    prop_republican = votes[3] / (votes[1] + votes[3]), # renormalizing after removing "other"
    
    response_regression = frac_votes[3]
  ) %>%
  dplyr::select(
    fips, 
    state, 
    state_abbreviation, 
    county,
    
    votes,
    votes_total, 
    votes_democrat,
    votes_other,
    votes_republican,
    
    frac_votes,
    frac_democrat,
    frac_other,
    frac_republican,
    
    prop_democrat,
    prop_republican,
    
    party,
    party_frac_lead,
    party_won,
    
    response_binary,
    response_factor,
    response_regression
  )

elections_summary <- elections

elections %<>% 
  group_by(fips) %>%
  filter(row_number() == 1)

# make sure that binary baseline is yes, since we will try to model that
elections$response_factor <- factor(elections$response_factor, levels=c("yes", "no"), ordered=FALSE)

