default_font_color <- "#4e4d47"
default_background_color <- "#f5f5f2"
default_font_family <- "Arial"

color_scale <- tibble(
  "democrat - low" = "#C0CCDD",
  "democrat - med_low" = "#a9bbd3",
  "democrat - med" = "#819ABB",
  "democrat - med_high" = "#26659b",
  "democrat - high" = "#0e4375",

  "republican - low" = "#ddbdbd",
  "republican - med_low" = "#db8f7f",
  "republican - med" = "#cf6a55",
  "republican - med_high" = "#c95d47",  
  "republican - high" = "#c32b0d"
) %>% gather("group", "fill")

loadfonts()
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial Unicode MS",
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#e8e8e5", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}
