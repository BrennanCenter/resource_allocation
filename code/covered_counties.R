## list from here: https://www.justice.gov/crt/jurisdictions-previously-covered-section-5
vra_counties <- fread("./raw_data/misc/vra_covered_counties.csv") %>% 
  mutate(covered = T)

counties <- left_join(fips_codes, vra_counties, by = c("state", "county" = "name"))

counties <- counties %>% 
  mutate(covered = ifelse(is.na(covered), state %in% c("AL", "AK", "AZ", "GA", "LA", "MS", "SC", "TX", "VA"), covered))

### TAKE OUT THE VA COUNTIES THAT WERE NO LONGER COVERED
### https://www.justice.gov/crt/section-4-voting-rights-act#bailout_list

bailed_out_va <- c("Frederick", "Shenandoah", "Roanoke", "Rockingham", "Warren", "Greene", "Pulaski", "Augusta",
                     "Botetourt", "Essex", "Middlesex", "Amherst", "Page", "Washington", "Rappahannock", "Bedford", "Culpeper",
                     "James City", "King George", "Prince William", "Wythe", "Grayson", "Craig", "Carroll")

bailed_out_va <- paste(bailed_out_va, "County")

counties$covered <- ifelse(counties$state == "VA" & counties$county %in% bailed_out_va, F, counties$covered)

counties$county_fips <- paste0(counties$state_code, counties$county_code)

saveRDS(counties, "./temp/covered.rds")