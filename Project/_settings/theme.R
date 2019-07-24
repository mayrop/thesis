# Loading fonts if required
if (config$settings$load_fonts) {
  loadfonts()
}

color_scale <- as_tibble(
    config$maps$colors
  ) %>% 
  mutate_if(is.factor, as.character) %>% 
  gather("group", "fill")

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(
        family = config$theme$font_family,
        color = config$theme$font_color
      ),
      
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      
      # add a subtle grid
      panel.grid.major = element_line(
        color = config$theme$border_color, 
        size = 0.2
      ),
      
      panel.grid.minor = element_blank(),
      
      # background colors
      plot.background = element_rect(
        fill = config$theme$bg_color,
        color = NA
      ),
      
      panel.background = element_rect(
        fill = config$theme$bg_color,
        color = NA
      ),
      
      legend.background = element_rect(
        fill = config$theme$bg_color,
        color = NA
      ),
      
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),

      # titles
      legend.title = element_text(size = 11),
      
      legend.text = element_text(
        size = 9, 
        hjust = 0,
        color = config$theme$font_color
      ),
      
      plot.title = element_text(
        size = 15, 
        hjust = 0.5,
        color = config$theme$font_color
      ),
      
      ...
    )
}
