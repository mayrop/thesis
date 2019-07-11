########################################
# Map by final election by county
spatial_data <- inner_join(all, get_urbn_map(map = "counties", sf = TRUE),
                           by = c("fips" = "county_fips"))

# counties <- get_urbn_map(map = "counties", sf = TRUE)
# missing<- counties[!(counties$county_fips %in% all$fips),]

#Error: stat_sf requires the following missing aesthetics: geometry
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = map_color)) + 
  scale_fill_manual(values = c("#db8f7f", "#cf6a55", "#c4462d", "#c32b0d","#C0CCDD", "#819ABB", "#3D6C99", "#0e4375"))

spatial_data %>% 
  filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = party_won)) + 
  scale_fill_manual(values = c("#0e4375", "#c32b0d"))

########################################
# Map by final election by county
spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = education_bachelor_percent_2013), color = "#ffffff", size = 0.001)

spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = housing_units_in_multiunit_2013), color = "#ffffff", size = 0.01)

spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = race_white_no_hispanic_percent_2014), color = "#ffffff", size = 0.01)

########################################

