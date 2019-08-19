# Working with the facts

facts <- facts %>%
  mutate(
    fips = str_pad(fips, 5, pad="0"),
    state_facts = state_abbreviation
  ) %>%
  dplyr::select(
    -state_abbreviation
  )

# This is for categorizing certain variables
levels <- c("low", "med", "high")

# Renaming variables for better readibility
facts <- facts %>%
  mutate(
    # population
    pop_14 = PST045214 / 1e3,
    
    pop_density_10 = POP060210,
    
    pop_10 = POP010210,
    pop_pct_change = PST120214,
    pop_same_house_pct_13 = POP715213, 
    pop_foreign_pct_13 = POP645213, 
    pop_o_lang_pct_13 = POP815213,
    
    # age
    age_u5_pct_14 = AGE135214,
    age_u18_pct_14 = AGE295214,
    age_o65_pct_14 = AGE775214,
    
    # education
    edu_hs_pct_13 = EDU635213,
    edu_bach_pct_13 = EDU685213,

    # race
    rh_white_pct_14 = RHI125214, 
    rh_afroamerican_pct_14 = RHI225214,
    rh_latino_pct_14 = RHI725214,
    rh_white_nohisp_pct_14 = RHI825214,
    rh_native_pct_14 = RHI325214,
    rh_asian_pct_14 = RHI425214,
    rh_hawaiian_pct_14 = RHI525214,
    rh_two_races_pct_14 = RHI625214,

    # sex
    females_pop_14 = PST045214 * SEX255214 / 100,
    females_pct_14 = SEX255214,
    
    # housing & buildings & businesses
    bldg_permits_rate_14 = BPS030214 / PST045214,
    
    # non farm
    nf_priv_establ_rate_13 = BZA010213 / PST045214,
    nf_priv_emplt_rate_13 = BZA110213 / PST045214,
    
    # private nonfarm employment
    nf_priv_emplt_pct_chg_13 = BZA115213, 
    
    # non employer
    nonemployer_establ_rate_13 = NES010213 / PST045214,
    
    # housing
    hsg_units_rate_14 = HSG010214 / PST045214,
    hsg_multiunit_pct_13 = HSG096213, 
    hsg_homeowner_rate_13 = HSG445213, 
    hsg_val_units_13 = HSG495213 / 1e3,

    hsd_persons_per_household_13 = HSD310213, 
    hsd_household_rate_13 = HSD410213 / PST045214, 

    # money
    inc_med_househ_income_13 = INC110213 / 1e3,
    inc_pc_12_month_13 = INC910213 / 1e3,
    inc_pers_blw_povt_pct_13 = PVY020213,
    
    # veterans
    veterans_pct_13 = (VET605213 * 100) / PST045214
  ) %>%
  dplyr::select(
    -PST045214,
    -POP060210,
    
    -POP010210,
    -PST120214,
    -POP715213, 
    -POP645213, 
    -POP815213,
    
    # age
    -AGE135214,
    -AGE295214,
    -AGE775214,
    
    # education
    -EDU635213,
    -EDU685213,
    
    # race
    -RHI125214, 
    -RHI225214,
    -RHI725214,
    -RHI825214,
    -RHI325214,
    -RHI425214,
    -RHI525214,
    -RHI625214,
    
    # sex
    -SEX255214,
    -females_pop_14,
    
    # non farm
    -BZA010213,
    -BZA110213,
    
    # private nonfarm employment
    -BZA115213, 
    
    -NES010213,
    
    # household
    -HSG010214,
    -HSD310213, 
    -HSG096213, 
    -HSG445213, 
    -HSD410213, 
    -HSG495213,
    
    # Businesses    
    -SBO001207,
    -SBO115207,
    -SBO215207,
    -SBO315207,
    -SBO415207,
    -SBO515207,
    -SBO015207,
    
    # money
    -INC110213,
    -INC910213,
    -PVY020213,
    
    # random
    -LFE305213,
    -LND110210,
    -AFN120207,
    -WTN220207,
    -MAN450207,
    -RTN130207,
    -RTN131207,
    -BPS030214,
    
    # veterans
    -VET605213
  )

#######

# separating
facts_states <- facts %>%
  dplyr::filter(state_facts == "")

facts <- facts %>%
  dplyr::filter(state_facts != "")


