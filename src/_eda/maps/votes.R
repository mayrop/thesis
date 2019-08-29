
########################################
# Troubleshooting:
#
# Error: stat_sf requires the following missing aesthetics: geometry
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

max_size <- max(sqrt(spatial_data$votes_democrat), sqrt(spatial_data$votes_republican))
min_size <- min(sqrt(spatial_data$votes_democrat), sqrt(spatial_data$votes_republican))

b <- 15
a <- 0.001

alpha_b <- 0.9
alpha_a <- 0.5

my_map <- spatial_data %>%
  mutate(
    longitude = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    latitude = map_dbl(geometry, ~st_centroid(.x)[[2]]),
    size = ifelse(party_won == "republican", sqrt(votes_republican), sqrt(votes_democrat)),
    # Here we try to normalize the size by a skewed density of votes by party
    # The reason is to emphasize the difference in votes
    size_normalized = (b-a) * ((size - min_size) / (max_size - min_size)) + a,
    alpha_size_normalized = (alpha_b-alpha_a) * ((size - min_size) / (max_size - min_size)) + alpha_a,
    color = party_won
  ) %>%
  ungroup() %>%
  select(
    longitude,
    latitude,
    size,
    size_normalized,
    alpha_size_normalized,
    color
  )

ggplot() +
  scale_alpha(
    name = "",
    range = c(0.7, 0),
    guide = FALSE
  ) +
  # Adding counties
  geom_sf(
    data = spatial_data %>% filter(state != "Alaska"),
    color = "gray",
    fill = "white",
    size = 0.1
  ) + 
  # Adding states for the borders
  geom_sf(
    data = states_data %>% filter(state_name != "Alaska"),
    aes(), 
    fill = "transparent",
    color = "gray", 
    size = 0.35
  ) +   
  geom_point(
    data = my_map,
    aes(
      x = longitude,
      y = latitude,
      color = color      
    ), 
    size = my_map$size_normalized,
    alpha = my_map$alpha_size_normalized
  ) +
  scale_color_manual(
    values = config$theme$parties_colors,
    labels = config$theme$parties_labels,
    guide = "legend"
  ) +
  labs(
    x = "",
    y = "",
    color = "Winning Candidate"
  ) + 
  theme_bw() + 
  theme(
    legend.position = c(0.01, 0.01),
    legend.justification = c("left", "bottom"),
    legend.text = element_text(size = 18),   
    legend.title = element_text(size = 20),  
    #legend.key.size = unit(5,"cm"),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()    
  ) +
  # changes the size of the points in legend
  guides(color = guide_legend(
    override.aes = list(size=5))
  )

#########################################
# Cleaning global environment
rm(my_map)
rm(alpha_a)
rm(alpha_b)
rm(max_size)
rm(min_size)
rm(a)
rm(b)
