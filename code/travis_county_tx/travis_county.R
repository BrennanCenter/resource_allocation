g_vote <- dbGetQuery(db, "select * from tx_roll_history_0419 where ELECTION_DATE == '20181106'")


# tx <- dbGetQuery(db, "select VUID, LAST_NAME, GENDER, DOB, PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_STREET_NAME, PERM_STREET_TYPE,
#                         PERM_CITY, PERM_ZIPCODE, PERM_DIRECTIONAL_PREFIX, PERM_DIRECTIONAL_SUFFIX, COUNTY_CODE
#                         from tx_roll_0419 where COUNTY_CODE == 227")
# 
# ## set up and geocode voter file
# tx <- tx %>% 
#   mutate_at(vars(PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_DIRECTIONAL_PREFIX, PERM_STREET_NAME, PERM_STREET_TYPE, PERM_DIRECTIONAL_SUFFIX), funs(ifelse(is.na(.), "", .))) %>% 
#   mutate(street = paste(PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_DIRECTIONAL_PREFIX, PERM_STREET_NAME, PERM_STREET_TYPE, PERM_DIRECTIONAL_SUFFIX),
#          street = gsub("\\s+", " ", street),
#          city = PERM_CITY,
#          zip = PERM_ZIPCODE,
#          state = "TX") %>% 
#   select(-starts_with("PERM")) %>% 
#   filter(!is.na(VUID))
# 
# tx <- geocode(tx)
# saveRDS(tx, "./temp/travis_county_geo.rds")

travis_county_vf <- readRDS("./temp/travis_county_geo.rds") %>% 
  filter(match != "No Match")

travis_county_vf$voted <- travis_county_vf$VUID %in% g_vote$VUID

###

polls <- fread("./raw_data/misc/travis_county_polls.csv") %>% 
  rename(street = Address,
         city = City,
         state = State,
         zip = Zipcode)
polls <- geocode(polls)

manual <- fread("./raw_data/misc/travis_county_polls_manual_geo.csv")

polls <- left_join(polls, manual)

if(nrow(filter(polls, match == "No Match", is.na(latitude_man))) > 0) stop("something didn't geocode")
rm(manual)

polls <- polls %>% 
  mutate(latitude = ifelse(is.na(latitude_man), latitude, latitude_man),
         longitude = ifelse(is.na(longitude_man), longitude, longitude_man)) %>% 
  select(-longitude_man, -latitude_man)


voters <- SpatialPoints(travis_county_vf[c('longitude','latitude')])
poll_locs <- SpatialPoints(polls[c('longitude','latitude')])
travis_county_vf$nearest_poll <- apply(gDistance(poll_locs, voters, byid=TRUE), 1, which.min)
rm(voters, poll_locs)

polls$id <- c(1:nrow(polls))
colnames(polls) <- make.names(colnames(polls))
polls <- select(polls, id, BuildingName, booths = X..Booths)

####

travis_county_vf <- left_join(travis_county_vf, polls, by = c("nearest_poll" = "id"))
tracts <- readOGR("./raw_data/shapefiles/tl_2018_48_tract", "tl_2018_48_tract")

pings  <- SpatialPoints(travis_county_vf[c('longitude','latitude')], proj4string = tracts@proj4string)
travis_county_vf$tract <- over(pings, tracts)$GEOID

tract_data <- get_basic_census_stats(geo = "tract", state = "TX", county = "TRAVIS", year = 2017)
tract_data <- left_join(tract_data, select(tracts@data, GEOID, ALAND), by = "GEOID") %>% 
  mutate(pop_dens = population / as.integer(as.character(ALAND)))

travis_county_vf <- left_join(travis_county_vf, tract_data, by = c("tract" = "GEOID"))

### voters per polling place

poll_demos <- travis_county_vf %>% 
  group_by(nearest_poll, booths) %>% 
  summarize(pp_voter_count = n(),
            pp_votes_count = sum(voted),
            pp_median_income = mean(median_income, na.rm = T),
            pp_latino = mean(latino, na.rm = T),
            pp_nh_white = mean(nh_white, na.rm = T)) %>% 
  mutate(pp_voters_per_booth = pp_voter_count / booths,
         pp_votes_per_booth = pp_votes_count / booths)

###

travis_county_vf <- left_join(travis_county_vf, poll_demos, by = c("nearest_poll", "booths"))

## surname analysis
## use wru to come up with race estimates
# tx_census <- get_census_data(key = api_key, state = "TX", age = F, sex = F, census.geo = "tract")
# saveRDS(tx_census, "./temp/wru_census_tx.RDS")

tx_census <- readRDS("./temp/wru_census_tx.RDS")

travis_county_vf <- travis_county_vf %>%
  rename(surname = LAST_NAME,
         tract_full = tract) %>%
  mutate(state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "TX",
         yob = floor(as.integer(travis_county_vf$DOB) / 10000))

travis_county_vf <- predict_race(travis_county_vf, census.geo = "tract", census.key = api_key, retry = 999, census.data = tx_census)

tract_area <- select(tracts@data, GEOID, ALAND)

summary(lm(pp_voter_count ~ median_income, data = travis_county_vf))
summary(lm(pp_voters_per_booth ~ median_income, data = travis_county_vf))
summary(lm(pp_votes_per_booth ~ pred.bla + pred.his + pred.whi +
             as.factor(GENDER) + as.integer(substring(DOB, 1, 4)) +
             I(median_income / 1000) + pop_dens, data = filter(travis_county_vf, voted)))