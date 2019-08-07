########################################
# Troubleshooting:
#
# Error: stat_sf requires the following missing aesthetics: geometry
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

spatial_data %>% 
  ggplot() + 
  geom_sf(
    aes(
      fill = party_won
    ), 
    color = "white",
    size = 0.1
  ) + 
  labs(
    fill="Winning Party"
  ) +  
  geom_sf(
    data = states_data,
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
    values = rev(config$theme$parties_colors), 
    labels = rev(config$theme$parties_labels)
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
    legend.background = element_rect(fill = "white"),
    legend.justification = c("left", "bottom")
  )
