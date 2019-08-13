

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
    counties_pct = counties/sum(counties) * 100
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

skim_with_defaults()
skim_with(numeric = list(mean = mean, sd = sd), append = FALSE)
skim_with(integer = list(mean = mean, sd = sd), append = FALSE)

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
    inc_pc_past_12_month_13,
    age_o65_pct_14
  ) %>% 
  mutate (
    pop_10 = pop_10 / 1e3,
    pop_14 = pop_14 / 1e3,
    hsg_val_units_13 = hsg_val_units_13 / 1e3
  ) %>%
  skim_to_list() %>%
  rbindlist(.) %>% 
  mutate_if(
    is.character, trimws
  ) %>%
  unite(., "stats", c("mean", "sd")) %>%
  spread(party, stats) %>%
  select(
    variable,
    republican,
    democratic
  ) %>%
  separate(democratic, c("democratic_mean", "democratic_sd"), sep="_") %>% 
  separate(republican, c("republican_mean", "republican_sd"), sep="_") %>%  
  mutate_at(
    vars(2:5), as.numeric
  ) %>% 
  mutate_if(
    is.numeric, round, digits = 1
  ) %>%
  kable(
    caption = 'My caption here', 
    booktabs = TRUE, 
    format = "latex"
  ) %>%
  kable_styling(font_size = 12, latex_options = "HOLD_position") %>%
  add_header_above(c(" ", "Republican" = 2, "Democratic" = 2))
