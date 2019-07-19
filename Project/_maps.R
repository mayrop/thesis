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
    size=ifelse(party_won=="democrat",votes_democrat,votes_republican),
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

b<-15
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
  geom_sf() + 
  geom_point(
    data=my_map,
    aes(
      x=longitude,
      y=latitude,
      color=color      
    ), size=my_map$size_normalized,
    alpha = my_map$alpha_size_normalized
  ) +
  scale_color_manual(values=c("#0e4375", "#c32b0d")) +
  theme_map()

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



pop_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = ifelse(gini=="democrat", 0, 1),
         mean = ifelse(mean=="low", 0, ifelse(mean=="medium", 1, 2)))

legend <- ggplot() +
  geom_tile(
    data = pop_color_scale,
    mapping = aes(
      x = gini,
      y = mean,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher inequality →️️",
       y = "Higher income →️") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()

ggdraw() + draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
  

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

