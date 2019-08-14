# First summarize the votes
summary_votes <- elections_summary %>%
  dplyr::group_by(party) %>%
  dplyr::summarize(
    votes = sum(votes) / 1e6
  )

# Then summarize counties (excluding Alaska)
summary_counties <- elections_summary %>%
  dplyr::group_by(party) %>%
  dplyr::filter(
    state_abbreviation != "AK"
  ) %>%
  dplyr::summarize(
    counties = sum(party_won == party)
  )

# Then join both summaries
summary <- summary_votes %>%
  dplyr::left_join(summary_counties, by="party") %>%
  dplyr::mutate(
    votes_pct = votes / sum(votes) * 100,
    counties_pct = counties / sum(counties) * 100
  ) %>%
  dplyr::mutate_if(
    is.numeric, round, digits = 10
  ) %>%
  tidyr::unite(., "Popular Vote, (millions)", c("votes", "votes_pct")) %>%
  tidyr::unite(., "Counties Won[note]", c("counties", "counties_pct"))

# transpose the data frame
summary <- dcast(
  melt(as.data.frame(summary), id.vars = "party"), variable ~ party
)

my_summaries[["elections"]] <- summary %>% 
  select(
    variable,
    republican,
    democrat,
    other
  )

my_summaries[["elections"]] %>%
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
my_summaries[["facts"]] <- all %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    party = ifelse(response_factor == "yes", "republican", "democratic")
  ) %>%
  dplyr::group_by(
    party
  ) %>%
  dplyr::select(
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
  dplyr::mutate(
    pop_10 = pop_10 / 1e3,
    pop_14 = pop_14 / 1e3,
    hsg_val_units_13 = hsg_val_units_13 / 1e3
  ) %>% 
  dplyr::group_by(party) %>%
  # this can be approached by skim_to_list (skimr package)
  # but we try to minimize the third party packages
  dplyr::summarise_all(
    list(mean = mean, median = median, sd = sd)
  ) %>%
  tidyr::gather(
    "var", "val", -party
  ) %>%
  # this is where magic happens...
  dplyr::mutate(
    stat = gsub("^[a-z0-9_]+_([a-z]+)$", "\\1", var),
    var = gsub("^([a-z0-9_]+)_[a-z]+$", "\\1", var)
  ) %>%
  dplyr::group_by(
    party, var
  ) %>% 
  tidyr::spread(
    stat, val
  ) %>% 
  dplyr::group_by(var) %>%
  tidyr::unite(temp, mean, median, sd) %>%
  tidyr::spread(party, temp) %>%
  tidyr::separate(
    democratic, 
    into = paste("dem", c("mean", "median", "sd"), sep = "_"), 
    sep = "_"
  ) %>%
  tidyr::separate(
    republican, 
    into = paste("rep", c("mean", "median", "sd"), sep = "_"), 
    sep = "_"
  ) %>%
  dplyr::mutate_at(
    vars(-1), as.numeric
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate_if(
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
