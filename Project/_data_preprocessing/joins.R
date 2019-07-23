# Main Join for both datasets

all <- full_join(
  elections, facts, by="fips"
)

# Handling the data for maps

map_columns <- list(
  "population" = "pop_14_level",
  "education" = "edu_bachelor_pct_13_level",
  "race_white" = "race_white_no_hisp_pct_14_level",
  "housing_units" = "housing_units_in_multiunit_13_level"
)

for (column in names(map_columns)) {
  new_col = paste("map_", column, sep="")
  new_col_fill = paste("map_fill_", column, sep="")
  
  all <- all %>%
    mutate(
      # use !! for dynamic variable name assignment
      # use !!rlang::sym for dynamic variable name to get from data
      !!new_col := paste(
        party_won, "-", !!rlang::sym(map_columns[[column]])
      )
    ) %>%
    left_join(
      color_scale %>% rename(!!new_col_fill := fill), by = setNames("group", new_col)
    )
}

################################################
################################################

# Removing incomplete rows
all <- all[complete.cases(all),]

# It removes AK rows
nrow(all)
# 3112 

spatial_data <- inner_join(all, get_urbn_map(map = "counties", sf = TRUE),
                           by = c("fips" = "county_fips"))

