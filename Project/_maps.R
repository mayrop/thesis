########################################
# Map by final election by county
spatial_data <- inner_join(all, get_urbn_map(map = "counties", sf = TRUE),
                           by = c("fips" = "county_fips"))

# counties <- get_urbn_map(map = "counties", sf = TRUE)
# missing<- counties[!(counties$county_fips %in% all$fips),]

spatial_data %>% 
  filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = map_color)) + 
  scale_fill_manual(values = c("#db8f7f", "#cf6a55", "#c4462d", "#c32b0d","#C0CCDD", "#819ABB", "#3D6C99", "#0e4375"))

spatial_data %>% 
  filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = party_won)) + 
  scale_fill_manual(values = c("#c32b0d","#0e4375"))

########################################
# Map by final election by county
spatial_data %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = education_high_school_percent_2013), color = "#ffffff", size = 0.001)
########################################

