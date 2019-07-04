# Working with the facts

facts_filtered <- facts_orig %>%
  mutate(
    fips = str_pad(fips, 5, pad="0"),
    state_facts = state_abbreviation
  ) %>%
  filter(
    state_facts != "",
    PST045214 != 0 # fips == 51515, this is not in elections, so not a problem
  ) %>%
  dplyr::select(
    -state_abbreviation
  )

# Double checking
facts_filtered %>% 
  dplyr::select(-fips, -area_name, -state_facts) %>% 
  skim()

# Renaming variables for better readibility
facts_vars <- facts_filtered %>%
  mutate(
    # population
    population_2014 = PST045214,
    population_density_2010 = POP060210,
    
    population_2010 = POP010210,
    population_percent_change = PST120214,
    population_same_house_percent_2013 = POP715213, 
    population_foreign_percent_2013 = POP645213, 
    population_other_language_percent_2013 = POP815213,
    
    # age
    age_under_5_percent_2014 = (AGE135214 * 100) / PST045214,
    age_under_18_percent_2014 = (AGE295214 * 100) / PST045214,
    age_over_65_percent_2014 = (AGE775214 * 100) / PST045214,
    
    # education
    education_high_school_percent_2013 = EDU635213,
    education_bachelor_percent_2013 = EDU685213,
    
    # race
    race_white_percent_2014 = RHI125214, 
    race_afroamerican_percent_2014 = RHI225214,
    race_latino_percent_2014 = RHI725214,
    race_white_no_hispanic_percent_2014 = RHI825214,
    race_native_percent_2014 = RHI325214,
    race_asian_percent_2014 = RHI425214,
    race_hawaiian_percent_2014 = RHI525214,
    race_two_races_percent_2014 = RHI625214,
    
    # sex
    females_percent_2014 = SEX255214,
    
    # housing & buildings & businesses
    hbb_building_permits_rate_2014 = BPS030214 / PST045214,
    
    # non farm
    nonfarm_private_establishments_rate_2014 = BZA010213 / PST045214,
    nonfarm_private_employment_rate_2013 = BZA110213 / PST045214,
    nonfarm_private_employment_percent_change_2013 = BZA115213, # private nonfarm employment
    
    # non employer
    nonemployer_establishments_rate_2013 = NES010213 / PST045214,
    
    # household
    housing_units_rate_2014 = HSG010214 / PST045214,
    housing_person_per_household_2013 = HSD310213, 
    housing_units_in_multiunit_2013 = HSG096213, 
    housing_homeownership_rate_2013 = HSG445213, 
    housing_households_rate_2013 = HSD410213 / PST045214, 
    housing_median_value_in_housing_units_2013 = HSG495213,
    
    # Businesses    
    businesses_rate_2007 = SBO001207 / PST045214,
    businesses_indian_rate_2007 = SBO115207 / PST045214,
    businesses_asian_rate_2007 = SBO215207 / PST045214,
    businesses_afroamerican_rate_2007 = SBO315207 / PST045214,
    businesses_hispanic_rate_2007 = SBO415207 / PST045214,
    businesses_hawaiian_rate_2007 = SBO515207 / PST045214,
    businesses_women_rate_2007 = SBO015207 / PST045214,
    
    # money
    income_median_household_income_2013 = INC110213,
    income_per_capita_income_past_12_month_2013 = INC910213,
    income_persons_below_poverty_percent_2013 = PVY020213,
    
    # random
    random_travel_mean_time_2013 = LFE305213,
    random_land_area_in_miles_2010 = LND110210,
    random_accomodation_and_food_sales_rate_2007 = AFN120207 / PST045214,
    random_merchant_sales_rate_2007 = WTN220207 / PST045214,
    random_manufacters_shipments_sales_rate_2007 = MAN450207 / PST045214,
    # random_retail_sales_2007 = RTN130207,
    random_retail_sales_rate_2007 = RTN131207 / PST045214,
    random_retail_sales_per_capita_2007 = RTN131207,
    
    # veterans
    veterans_percent_2013 = (VET605213 * 100) / PST045214
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
    
    # non farm
    -BZA010213,
    -BZA110213,
    -BZA115213, # private nonfarm employment
    
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
    -VET605213,
    
    # reduntant
      # reduntant with population_2014 and population_density_2010
    -population_2010,
    
      # reduntant with race_white_no_hispanic_percent_2014
    -race_white_percent_2014,
    
      # reduntant with population_foreign_percent_2013
    -population_other_language_percent_2013,

      # reduntant with nonfarm_private_employment_percent_change_2013 
      # & nonfarm_private_establishments_rate_2014
    -nonfarm_private_employment_rate_2013 
  )

# Analysis
facts_vars %>% 
  dplyr::select(-fips, -area_name, -state_facts) %>% 
  skim()

# Correlations
