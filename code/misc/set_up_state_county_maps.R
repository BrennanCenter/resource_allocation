#### set up county shapefiles for mapping

counties_maps <- readOGR("./raw_data/shapefiles/cb_2017_us_county_20m",
                    "cb_2017_us_county_20m")
counties_maps <- spTransform(counties_maps, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
counties_maps@data$id <- rownames(counties_maps@data)

### move alaska hawaii
alaska <- counties_maps[counties_maps$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(counties_maps)

hawaii <- counties_maps[counties_maps$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(counties_maps)

counties_maps <- counties_maps[(as.integer(as.character(counties_maps$STATEFP)) <= 56) & (!counties_maps$STATEFP %in% c("02", "15")), ]

counties_maps <- rbind(counties_maps, hawaii, alaska)
####
counties_df <- fortify(counties_maps)
counties_maps <- left_join(counties_df, counties_maps@data, by = "id") %>% 
  mutate(GEOID = as.character(GEOID))
rm(counties_df)

#### set up state shapefiles for mapping

states_maps <- readOGR("./raw_data/shapefiles/cb_2017_us_state_20m",
                    "cb_2017_us_state_20m")
states_maps <- spTransform(states_maps, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
states_maps@data$id <- rownames(states_maps@data)

### move alaska hawaii
alaska <- states_maps[states_maps$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(states_maps)

hawaii <- states_maps[states_maps$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(states_maps)

states_maps <- states_maps[(as.integer(as.character(states_maps$STATEFP)) <= 56) & (!states_maps$STATEFP %in% c("02", "15")), ]

states_maps <- rbind(states_maps, hawaii, alaska)
####
states_df <- fortify(states_maps)
states_maps <- left_join(states_df, states_maps@data, by = "id") %>% 
  mutate(GEOID = as.character(GEOID))
rm(states_df)
rm(alaska, hawaii)