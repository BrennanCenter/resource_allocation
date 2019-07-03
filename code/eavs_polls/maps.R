### change over time

all_data <- readRDS("./temp/full_eavs.rds")


t <- dcast(setDT(all_data), FIPSCode + State + toupper(JurisdictionName) ~ year,
           value.var = c("total_precincts", "total_polling_places", "total_workers", "workers_under_18", "workers_18_25",
                         "workers_26_40", "workers_41_60", "workers_61_70", "workers_over_70",
                         "difficulty_find_workers", "early_vote_polling", "ed_vote_polling", 
                         "in_person_ballots_ed", "in_person_ballots_early")) %>% 
  mutate(change_polling_2008_2016 = (total_polling_places_2016 - total_polling_places_2008) / total_polling_places_2008,
         voters_per_worker = in_person_ballots_ed_2018 / total_workers_2018,
         polling_place_voter = in_person_ballots_ed_2018 / ed_vote_polling_2018,
         change_early_sites = (early_vote_polling_2018 - early_vote_polling_2014) / early_vote_polling_2014,
         change_ed_sites = (ed_vote_polling_2018 - ed_vote_polling_2014) / ed_vote_polling_2014,
         net_ed_sites = ifelse(total_polling_places_2018 > 0 & total_polling_places_2014 > 0,
                               total_polling_places_2018 - total_polling_places_2014,
                               NA))

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
                       t %>% 
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
  geom_polygon(data = counties2, aes(x = long, y = lat, group = group, fill = polling_place_voter)) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  ggtitle("Voters per Polling Place, 2018") +
  scale_fill_gradient(high = "red", low = "green", limits = c(0, 1000), oob = squish, name = "Voters per Polling Place")
ggsave("./output/voters_per_pollingplace.png")
  
  
g <- ggplot() +
  theme(axis.ticks = element_blank(),
           axis.text = element_blank(),
           panel.background = element_blank(),
           panel.border = element_blank(),
           legend.position = "bottom",
           text = element_text(family = "Century Gothic"),
           plot.title = element_text(hjust = 0.5)) +
  geom_polygon_interactive(data = counties2, aes(x = long, y = lat, group = group, fill = polling_place_voter,
                                                 tooltip = paste0(JurisdictionName, "<br>", "Votes per Polling Place: ", comma(round(polling_place_voter)))),
                           color = "black", size = 0.01) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(high = "red", low = "green", limits = c(0, 750), oob = squish, name = "Voters per Polling Place")



saveWidget(ggiraph(code=print(g), width = .7), "interactive_map_less_detail_14_pp.html")

## change locations percent
ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Century Gothic"),
        plot.title = element_text(hjust = 0.5)) +
  geom_polygon(data = counties2, aes(x = long, y = lat, group = group, fill = net_ed_sites)) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  ggtitle("Change in Polling Place Count, 2014 \u2013 2018") +
  scale_fill_gradient(high = "green", low = "red", name = "Change in Polling Place", limits = c(-10, 10), oob = squish)
ggsave("./output/change_pps.png")
g <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Century Gothic"),
        plot.title = element_text(hjust = 0.5)) +
  geom_polygon_interactive(data = counties2, aes(x = long, y = lat, group = group, fill = net_ed_sites,
                                                 tooltip = paste0(JurisdictionName, "<br>", "Change in Polling Places: ", comma(round(net_ed_sites)))),
                           color = "black", size = 0.01) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(high = "green", low = "red", name = "Change in Polling Place", limits = c(-10, 10), oob = squish) +
  ggtitle("Change in Polling Places from 2016 to 2018")



saveWidget(ggiraph(code=print(g), width = .7), "change_in_pollingplaces.html")