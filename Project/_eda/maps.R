########################################
# Map by final election by county

#counties <- get_urbn_map(map = "counties", sf = TRUE)
#missing<- counties[!(counties$county_fips %in% all$fips),]

#Error: stat_sf requires the following missing aesthetics: geometry
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = map_color)) + 
  scale_fill_manual(values = c("#db8f7f", "#cf6a55", "#c4462d", "#c32b0d","#C0CCDD", "#819ABB", "#3D6C99", "#0e4375"))


annotations <- tibble(
  label = c(
    "Grey"
  ),
  arrow_from = c(
    "500000,500000" # grey
  ),
  arrow_to = c(
    "-500000,-500000" # grey
  ),
  curvature = c(
    0.2 # grey
  ),
  nudge = c(
    "0,0" # grey
  ),
  just = c(
    "0.5,0" # grey
  )
) %>%
  separate(arrow_from, into = c("x", "y"), sep = "\\,") %>%
  separate(arrow_to, into = c("x2", "y2"), sep = "\\,") %>%
  separate(nudge, into = c("nudge_x", "nudge_y"), sep = "\\,") %>%
  separate(just, into = c("hjust", "vjust"), sep = "\\,")

annotations[,-1] <- sapply(annotations[,-1],as.numeric)


my_map <- spatial_data %>%
  mutate(
    longitude=map_dbl(geometry, ~st_centroid(.x)[[1]]),
    latitude=map_dbl(geometry, ~st_centroid(.x)[[2]]),
    size=ifelse(party_won=="democrat",sqrt(votes_democrat),sqrt(votes_republican)),
    color=party_won
  )

my_map <- my_map %>%
  ungroup() %>%
  select(
    longitude,
    latitude,
    size,
    color
  )

b<-10
a<-0.001
min_size <- min(my_map$size)
max_size <- max(my_map$size)

my_map$size_normalized <- (b-a) * ((my_map$size - min_size) / (max_size - min_size)) + a

my_map$alpha_size_normalized <- (0.9-0.5) * ((my_map$size - min_size) / (max_size - min_size)) + 0.5


map <- ggplot(
    data = spatial_data
  ) +
  scale_alpha(
    name="",
    range=c(0.6,0),
    guide=F
  ) +
  geom_sf(
    color = "gray",
    size = 0.1
  ) + 
  geom_point(
    data=my_map,
    aes(
      x=longitude,
      y=latitude,
      color=color      
    ), 
    size=my_map$size_normalized,
    alpha = my_map$alpha_size_normalized
  ) +
  scale_color_manual(values=c("#0e4375", "#c32b0d")) +
  theme_map()


ggdraw() + draw_plot(map, 0, 0, 1, 1)

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
 
for (i in 1:nrow(annotations)) {
  current <- annotations[i,]
  
  map <- map +
    # for each annotation, add an arrow
    geom_curve(
      data = current,
      aes(
        x = x,
        xend = x2,
        y = y,
        yend = y2
      ),
      # that's the whole point of doing this loop:
      curvature = current %>% pull(curvature),
      size = 0.2,
      arrow = arrow(
        length = unit(0.005, "npc")
      )
    ) +
    # for each annotation, add a label
    geom_text(
      data = current,
      aes(
        x = x,
        y = y,
        label = label,
        hjust = hjust,
        vjust = vjust
      ),
      # that's the whole point of doing this loop:
      nudge_x = current %>% pull(nudge_x),
      nudge_y = current %>% pull(nudge_y),
      # other styles
      color = default_font_color,
      size = 3
    )
}


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
  data.frame(x=width*1.75, y=0, fill="transparent", label="$30K - $40K", w=width*3, h=width*2, size=3),
  data.frame(x=width*1.75, y=width, fill="transparent", label="$20K - $30K", w=width*3, h=width*2, size=3),
  data.frame(x=width*1.75, y=width*2, fill="transparent", label="$17K - $20K", w=width*3, h=width*2, size=3)
)
  
de2 <- rbind(
  data.frame(x=0, y=width/-1.35, fill="transparent", label="Clinton", w=width*3, h=width/2, size=3),
  data.frame(x=width, y=width/-1.35, fill="transparent", label="Trump", w=width*3, h=width/2, size=3)
)

color_scale <- rbind(color_scale, de)
  


arrow <- stri_unescape_unicode(gsub("\\U", "\\u", "\\U2192", fixed=TRUE))

legend <- ggplot(
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

