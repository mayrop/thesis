
########################################
# Troubleshooting:
#
# Error: stat_sf requires the following missing aesthetics: geometry
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

max_size <- max(sqrt(spatial_data$votes_democrat), sqrt(spatial_data$votes_republican))
min_size <- min(sqrt(spatial_data$votes_democrat), sqrt(spatial_data$votes_republican))

b <- 10
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
    alpha_size_normalized = (alpha_b-alpha_a) * ((my_map$size - min_size) / (max_size - min_size)) + alpha_a,
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

#png(filename="figures/map_popular_vote.png", width=250, height=180, bg="transparent", unit="mm", res=300)

ggplot(
    data = spatial_data
  ) +
  scale_alpha(
    name = "",
    range = c(0.7, 0),
    guide = FALSE
  ) +
  geom_sf(
    color = "gray",
    fill = "white",
    size = 0.1
  ) + 
  geom_sf(
    data = states_data,
    aes(), 
    fill = "transparent",
    color = "gray", 
    size = 0.25
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
    values = rev(config$theme$parties_colors),
    labels = rev(config$theme$parties_labels)
  ) +
  labs(
    color = "Winning Party"
  ) + 
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    # add a subtle grid
    panel.grid.major = element_line(
      color = config$theme$border_color, 
      size = 0.2
    ),    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.05, 0.05),
    legend.background = element_rect(fill="white"),
    legend.justification = c("left", "bottom")
  )

#dev.off()