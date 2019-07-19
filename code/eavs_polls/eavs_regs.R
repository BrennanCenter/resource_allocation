eavs_data <- readRDS("./temp/full_eavs.rds") %>% 
  filter(year %in% c(2016, 2018)) %>% 
  mutate(county_fips = substring(FIPSCode, 1, 5)) %>% 
  group_by(county_fips, year) %>% 
  summarize_at(vars(total_precincts, total_polling_places, total_workers, in_person_ballots_ed),
               sum, na.rm = T)

e2 <- readRDS("./temp/full_eavs.rds") %>% 
  filter(year %in% c(2016, 2018)) %>% 
  mutate(county_fips = substring(FIPSCode, 1, 5)) %>% 
  group_by(county_fips, year) %>% 
  summarize(difficulty_find_workers = max(difficulty_find_workers, na.rm = T)) %>% 
  mutate(difficulty_find_workers = ifelse(difficulty_find_workers %in% c(1, 2), 1,
                                          ifelse(difficulty_find_workers %in% c(4, 5), 3, 2)))

eavs_data <- full_join(eavs_data, e2)
rm(e2)

####
census <- kevostools::get_basic_census_stats(geo = "county", year = 2017)

eavs_data2 <- inner_join(eavs_data, census, by = c("county_fips" = "GEOID")) %>% 
  filter(year == 2018,
         total_polling_places > 0,
         in_person_ballots_ed > 0) %>% 
  mutate(voters_per_pp = in_person_ballots_ed / total_polling_places,
         state = substring(county_fips, 1, 2)

eavs_data3 <- inner_join(eavs_data, census, by = c("county_fips" = "GEOID")) %>% 
  filter(year == 2018,
         total_workers > 0,
         in_person_ballots_ed > 0) %>% 
  mutate(voters_per_worker = in_person_ballots_ed / total_workers,
         state = substring(county_fips, 1, 2),
         to = in_person_ballots_ed / population)

eavs_data4 <- inner_join(eavs_data, census, by = c("county_fips" = "GEOID")) %>% 
  filter(year == 2018,
         total_workers > 0,
         total_polling_places > 0) %>% 
  mutate(workers_per_site = total_workers / total_polling_places,
         state = substring(county_fips, 1, 2))

####
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
###
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
####

counties <- left_join(counties, eavs_data2, by = c("GEOID" = "county_fips"))

ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Century Gothic"),
        plot.title = element_text(hjust = 0.5)) +
  geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = voters_per_pp), color = "black") +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_equal() +
  labs(x = NULL, y = NULL)

#####

eavs_data2$state <- substring(eavs_data2$county_fips, 1, 2)

summary(lm(workers_per_site ~ median_income + nh_black + some_college + population + 
             as.factor(state), data = eavs_data4))