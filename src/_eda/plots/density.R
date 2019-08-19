# Here we plot the density plots
plot1 <- ggplot(all, aes_string(
    x = predictor, 
    fill = "party_won", 
    colour = "party_won")
  ) + 
  geom_hline(
    yintercept = 0,
    color = "gray",
    size = 0.2
  ) +  
  # Changing label & colors for fill
  scale_fill_manual(
    values = config$theme$parties_colors,
    labels = config$theme$parties_labels
  ) +
  # Changing label & colors for line...
  scale_colour_manual(
    values = config$theme$parties_colors,
    labels = config$theme$parties_labels
  ) +  
  labs(
    y = "Density",
    fill = "Winning Candidate", 
    colour = "Winning Candidate"
  ) +
  geom_density(
    alpha = .035
  ) + 
  theme_classic() +
  theme(
    legend.position = config$predictors$list[[predictor]]$plots$density,
    plot.title = element_text(face = "bold", size = 13),
    axis.ticks.x = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm")
  )

# Horizontal boxplots
plot2 <- all %>%
  ungroup() %>%
  select(
    predictor,
    party_won
  ) %>%
  ggplot(
    aes_string(
      x = "party_won", 
      y = predictor
    )
  ) +
  geom_boxplot(
    fill = config$theme$parties_colors,
    colour = config$theme$parties_colors,
    alpha = 0.4
  ) +
  xlab("") +
  coord_flip() +
  labs(
    y = config$predictors$list[[predictor]]$name
  ) +  
  # Changing label & colors for fill
  scale_x_discrete(
    labels = config$theme$parties_labels
  ) +  
  theme_classic() +
  theme(
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm")
  )

# Getting both plots together...
cowplot::plot_grid(
  plot1, plot2, 
  ncol = 1, 
  rel_heights = c(2, 1.5),
  align = 'v',
  axis = 'lr'
)  

#########################################
# Cleaning global environment
rm(plot1)
rm(plot2)