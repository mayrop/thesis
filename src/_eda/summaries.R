# First summarize the votes
summary_votes <- elections_summary %>%
  group_by(party) %>%
  summarize(
    votes = sum(votes) / 1e6
  )

# Then summarize counties (excluding Alaska)
summary_counties <- elections_summary %>%
  group_by(party) %>%
  filter(
    state_abbreviation != "AK"
  ) %>%
  summarize(
    counties = sum(party_won == party)
  )

# Then join both summaries
summary <- summary_votes %>%
  left_join(summary_counties, by="party") %>%
  mutate(
    votes_pct = votes / sum(votes) * 100,
    counties_pct = counties / sum(counties) * 100
  ) %>%
  mutate_if(
    is.numeric, round, digits = 10
  ) %>%
  unite(., "Popular Vote, (millions)", c("votes", "votes_pct")) %>%
  unite(., "Counties Won[note]", c("counties", "counties_pct"))

# transpose the data frame
summary <- dcast(
  melt(as.data.frame(summary), id.vars = "party"), variable ~ party
)

summary %>% 
  select(
    variable,
    republican,
    democrat,
    other
  ) %>%
  kable(
    caption = '2016 United States presidential election summary', 
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position") %>%
  footnote(
    fixed_small_size = TRUE,
    symbol_title = "Note:",
    symbol = c("Numbers exclude the state of Alaska.")
  ) 

# Now let's summarize the facts...
facts_summary <- all %>%
  ungroup() %>%
  mutate(
    party = ifelse(response_factor == "yes", "republican", "democratic")
  ) %>%
  group_by(
    party
  ) %>%
  select(
    party,
    pop_14,
    pop_10,
    pop_density_10,
    pop_foreign_pct_13,
    pop_o_lang_pct_13,
    edu_bach_pct_13,
    rh_white_pct_14,
    rh_white_nohisp_pct_14,
    rh_asian_pct_14,
    rh_afroamerican_pct_14,
    hsg_multiunit_pct_13,
    hsg_val_units_13,
    hsg_homeowner_rate_13,
    nf_priv_emplt_rate_13,
    inc_pc_12_month_13,
    age_o65_pct_14
  ) %>% 
  mutate (
    pop_10 = pop_10 / 1e3,
    pop_14 = pop_14 / 1e3,
    hsg_val_units_13 = hsg_val_units_13 / 1e3
  ) %>% 
  group_by(party) %>%
  # this can be approached by skim_to_list (skimr package)
  # but we try to minimize the third party packages
  summarise_all(
    list(mean = mean, sd = sd, median = median)
  ) %>%
  gather(
    "var", "val", -party
  ) %>%
  mutate(
    stat = gsub("^[a-z0-9_]+_([a-z]+)$", "\\1", var),
    var = gsub("^([a-z0-9_]+)_[a-z]+$", "\\1", var)
  ) %>%
  group_by(
    party, var
  ) %>% 
  spread(
    stat, val
  ) %>% 
  group_by(var) %>%
  unite(temp, mean, sd, median) %>%
  spread(party, temp) %>%
  separate(
    democratic, 
    into = paste("dem", c("mean", "median", "sd"), sep="_"), 
    sep = "_"
  ) %>%
  separate(
    republican, 
    into = paste("rep", c("mean", "median", "sd"), sep="_"), 
    sep = "_"
  ) %>%
  mutate_at(
    vars(-1), as.numeric
  ) %>% 
  ungroup() %>%
  mutate_if(
    is.numeric, plyr::round_any, accuracy = .001, f = floor
  )

#print <- kable %>%
#  kable(
#    caption = 'My caption here', 
#    booktabs = TRUE, 
#    format = "latex"
#  ) %>%
#  kable_styling(font_size = 12, latex_options = "HOLD_position") %>%
#  add_header_above(c(" ", "Republican" = 2, "Democratic" = 2))
