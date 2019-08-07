

#ggdraw() + 

#plot(map, 0, 0, 1, 1)

map <- ggplot(
    data = spatial_data
  ) +
  scale_alpha(
    name="",
    range=c(0.6,0),
    guide=F
  ) +  
  geom_sf(
    aes(
      fill=map_fill_population
    ),
    color = "white",
    size = 0.1
  ) +
  geom_sf(
    data = states_data,
    aes(), 
    fill = "transparent",
    color = "white", 
    size = 0.25
  ) + 
  coord_sf(crs = "+proj=merc +lat_1=25 +lat_2=50 +lon_0=-100") +
  scale_fill_identity(guide = "legend")

ggdraw() + draw_plot(map, 0, 0, 1, 1)
 
width = 0.35

color_scale <- tibble(
    "democrat - low" = "#C0CCDD",
    "democrat - med" = "#819ABB",
    "democrat - high" = "#0e4375",
  
    "republican - low" = "#ddbdbd",
    "republican - med" = "#cf6a55",
    "republican - high" = "#c32b0d"
  ) %>% 
  gather("group", "fill")  %>%
  separate(group, into = c("x", "y"), sep = " - ") %>%
  mutate(
    x = ifelse(x=="democrat", 0, width * 1.25),
    y = ifelse(y=="low", 0, ifelse(y=="med", width, width*2)),
    w = rep(width, 6),
    h = rep(width, 6),
    label = "",
    size = rep(1, 6)
  )
  

de <- rbind(
  data.frame(x=width*2, y=0, fill="transparent", label="$30K - $40K", w=width*3, h=width*2, size=3),
  data.frame(x=width*2, y=width, fill="transparent", label="$20K - $30K", w=width*3, h=width*2, size=3),
  data.frame(x=width*2, y=width*2, fill="transparent", label="$17K - $20K", w=width*3, h=width*2, size=3)
)
  
de2 <- rbind(
  data.frame(x=0, y=width/-1.35, fill="transparent", label="Clinton", w=width*3, h=width/2, size=4),
  data.frame(x=width*1.25, y=width/-1.35, fill="transparent", label="Trump", w=width*3, h=width/2, size=4)
)

color_scale <- rbind(color_scale, de)

arrow <- stri_unescape_unicode(gsub("\\U", "\\u", "\\U2192", fixed=TRUE))

legend <- x
  
  ggplot(
    data = color_scale,
    aes(
      x = x,
      y = y,
      width = w,
      height = h
    )) +
  geom_tile(
    aes(
      fill = fill
    )) +
  geom_text(
    data = de,
    aes(
      label = label,
      size = size
    ),
    hjust = 0
  ) +
  geom_text(
    data = de2,
    aes(
      label = label,
      size = size
    )
  ) +
  scale_fill_identity() +
  scale_size_identity() +
  #labs(
  #  y = paste("Highder income ", arrow)
  #) +
  theme_map() +
  # make font small enough
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  # quadratic tiles
  coord_fixed()

ggdraw() + draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.135, 0.2, 0.2)
  

 spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = party_won)) + 
  scale_fill_manual(values = c("#0e4375", "#c32b0d"))

########################################
# Map by final election by county
spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = log10(pop_14)))

spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = log2(pop_14)))

spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_14_level))




spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = housing_units_in_multiunit_2013), color = "#ffffff", size = 0.01)

spatial_data %>% 
  dplyr::filter(party=="republican") %>% 
  ggplot() + 
  geom_sf(aes(fill = race_white_no_hispanic_percent_2014), color = "#ffffff", size = 0.01)

########################################

