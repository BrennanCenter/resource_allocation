### maps
all_data <- readRDS("./temp/all_data.rds")
#####
states <- readOGR("./raw_data/shapefiles/cb_2017_us_state_20m",
                  "cb_2017_us_state_20m")
states@data$id <- rownames(states@data)

states <- spTransform(states, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

alaska <- states[states$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(states)

hawaii <- states[states$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(states)

states <- states[(as.integer(as.character(states$GEOID)) <= 56) & (!states$GEOID %in% c("02", "15")), ]

states <- rbind(states, hawaii, alaska)

states_df <- fortify(states)
states <- left_join(states_df, states@data, by = "id") %>% 
  mutate(GEOID = as.character(GEOID))

####

counties <- readOGR("./raw_data/shapefiles/cb_2017_us_county_20m",
                    "cb_2017_us_county_20m")
counties <- spTransform(counties, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
counties@data$id <- rownames(counties@data)

alaska <- counties[counties$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(counties)

hawaii <- counties[counties$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(counties)

counties <- counties[(as.integer(as.character(counties$STATEFP)) <= 56) & (!counties$STATEFP %in% c("02", "15")), ]

counties <- rbind(counties, hawaii, alaska)

counties_df <- fortify(counties)
counties <- left_join(counties_df, counties@data, by = "id") %>% 
  mutate(GEOID = as.character(GEOID))
rm(counties_df)
######
counties <- mutate(counties, GEOID = ifelse(STATEFP == "02", "02000", GEOID)) ## Alaska statewide info

counties2 <- left_join(counties,
                       all_data %>% 
                         filter(year == 2016) %>% 
                         mutate(GEOID = substring(FIPSCode, 1, 5)),
                       by = "GEOID")


ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Century Gothic"),
        plot.title = element_text(hjust = 0.5)) +
  geom_polygon(data = counties2, aes(x = long, y = lat, group = group, fill = votes_per_worker)) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "#bfbfbf", high = "red", limits = c(0, 250), oob = squish)

g <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  geom_polygon_interactive(data = counties2, aes(x = long, y = lat, group = group, fill = votes_per_worker,
                                                 tooltip = paste0(JurisdictionName, "<br>", "Votes per Worker: ", votes_per_worker)),
                           color = "black", size = 0.01) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "#bfbfbf", high = "red", limits = c(0, 250), oob = squish)


saveWidget(ggiraph(code=print(g)), "./output/interactive_map_less_detail.html")