########################################
# Troubleshooting:
#
# Error: stat_sf requires the following missing aesthetics: geometry
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

ggplot() + 
  # Adding counties for the binary colors
  geom_sf(
    data = spatial_data %>% filter(state != "Alaska"),
    aes(
      fill = party_won
    ), 
    color = "white",
    size = 0.1
  ) + 
  labs(
    fill="Winning Candidate"
  ) +
  # Adding states for the borders
  geom_sf(
    data = states_data %>% filter(state_name != "Alaska"),
    aes(), 
    fill = "transparent",
    color = "white", 
    size = 0.25
  ) +  
  scale_alpha(
    name = "",
    range = c(0.7,0),
    guide = FALSE
  ) +  
  scale_fill_manual(
    values = config$theme$parties_colors, 
    labels = config$theme$parties_labels
  ) +  
  theme_bw() + 
  theme(
    legend.position = c(0.05, 0.05),
    legend.justification = c("left", "bottom"),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
