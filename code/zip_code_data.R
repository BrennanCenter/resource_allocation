
#### pull zipcode census data
zip_o65 <- get_acs(geography = "zcta",
                   variables = c("B01001_020",
                                 "B01001_021",
                                 "B01001_022",
                                 "B01001_023",
                                 "B01001_024",
                                 "B01001_025",
                                 "B01001_044",
                                 "B01001_045",
                                 "B01001_046",
                                 "B01001_047",
                                 "B01001_048",
                                 "B01001_049"),
                   summary_var = "B01001_001") %>% 
  group_by(zip = GEOID) %>% 
  summarize(over65 = sum(estimate / summary_est, na.rm = T))

#
zip_median_income <- census_income(geo = "zcta", year = 2017) %>% 
  select(-NAME) %>% 
  rename(zip = GEOID)


#
english_spoken <- get_acs(geography = "zcta", variables = c("B06007_002", "B06007_004", "B06007_007"), summary_var = "B06007_001") %>% 
  group_by(zip = GEOID) %>% 
  summarize(share_english_very_well = sum(estimate / summary_est, na.rm = T))

#
pop <- get_acs(geography = "zcta", variables = "B01001_001", year = 2017) %>% 
  group_by(zip = GEOID) %>% 
  summarize(population = sum(estimate))
#
zip_area <- readOGR("./raw_data/shapefiles/tl_2018_us_zcta510", "tl_2018_us_zcta510")@data %>% 
  group_by(zip = ZCTA5CE10) %>% 
  summarize(area = sum(as.integer(ALAND10)))

## zip data

zips <- inner_join(zip_o65, inner_join(zip_median_income, inner_join(english_spoken, inner_join(pop, zip_area))))
zips$pop_den <- zips$population / zips$area

saveRDS(zips, "./temp/zip_data.rds")