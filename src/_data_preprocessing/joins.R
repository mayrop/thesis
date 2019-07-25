# Main Join for both datasets

all <- full_join(
  elections, facts, by="fips"
)

# Handling the data for maps

for (column in names(config$maps$mapping)) {
  new_col = paste("map_", column, sep="")
  new_col_fill = paste("map_fill_", column, sep="")
  
  all <- all %>%
    mutate(
      # use !! for dynamic variable name assignment
      # use !!rlang::sym for dynamic variable name to get from data
      !!new_col := paste(
        party_won, "_", !!rlang::sym(config$maps$mapping[[column]]), sep=""
      )
    ) %>%
    left_join(
      color_scale %>% rename(!!new_col_fill := fill), by=setNames("group", new_col)
    )
}

################################################
################################################

# Removing incomplete rows
# It removes AK rows
all <- all[complete.cases(all),]

nrow(all)
# 3112 

# This is the dataset for the maps

counties_data <- get_urbn_map(map = "counties", sf = TRUE) %>%
  mutate(
    fips = county_fips
  ) %>% 
  select(
    county_fips, geometry
  )

states_data <- get_urbn_map(map = "states", sf = TRUE)

spatial_data <- left_join(counties_data, all,
                          by = c("county_fips" = "fips"))

# TODO - Improve readability
spatial_data <- spatial_data[!is.na(spatial_data$state_abbreviation),]


#################################################################################
######### Cleanup
rm(column)
rm(new_col)
rm(new_col_fill)

