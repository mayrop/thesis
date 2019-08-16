# Main Join for both datasets
all <- full_join(
  elections, facts, by="fips"
)

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


