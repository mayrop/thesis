
elections_summary %>%
  filter(
    state_abbreviation!="AK"
  ) %>%
  group_by(party) %>%
  summarize(
    votes=sum(votes)/1e6,
    counties=sum(party_won==party)
  ) %>%
  mutate(
    votes_pct=votes/sum(votes) * 100,
    counties_pct=counties/sum(counties) * 100
  ) 
  #%>% 
  #rownames_to_column %>% 
  #gather(var, value, -rowname) %>% 
  #spread(rowname, value) %>% 
  #kable("latex", caption = '\\label{tab:summary1a} BLABLA', booktabs = T) %>% 
  #kable_styling(font_size = 10, latex_options = c("hold_position"))

all %>%
  group_by(party_won) %>%
  summarize(
    white_mean=mean(rh_white_no_hisp_pct_14),
    bachelor_mean=mean(edu_bachelor_pct_13),
    hsg_multiunit_mean=mean(hsg_units_in_multiunit_13),
    inc_pc_mean=mean(inc_pc_past_12_month_13),
    white_sd=mean(rh_white_no_hisp_pct_14),
    bachelor_sd=mean(edu_bachelor_pct_13),
    hsg_multiunit_sd=mean(hsg_units_in_multiunit_13),
    inc_pc_sd=mean(inc_pc_past_12_month_13)
  )
