

cces_2008 <- readstata13::read.dta13("./raw_data/cces/cces_2008_common.dta")

## set up conversion from answer to minutes
times <- data.frame("resp" = c("Not at all", "Less than 10 minutes", "10 to 30 minutes", "31 minutes to 60 minutes"),
                    "minutes" = c(0, 5, 20, 45))

long <- fread("./raw_data/misc/free_wait_08.csv")


### pull people who voted at polls
voted_at_polls <- cces_2008 %>% 
  filter(CC405 %in% c("In person on election day (at polling booth or precinct)", "In person before election day (early)"),
         !is.na(CC405),
         CC406 != "Don't know") %>% 
  select(weight = V201,
         wait_cat = CC406,
         wait_free = CC406_t,
         birthyr = V207,
         gender = V208,
         educ = V213,
         race = V211,
         marital = V214,
         zip_code = V202,
         county_fips = V270,
         state = V259,
         family_income = V246,
         vote_type = CC405,
         party = CC307) %>% 
  mutate(county_fips = str_pad(as.character(county_fips), width = 3, pad = "0", side = "left"))

voted_at_polls <- left_join(voted_at_polls,
                            select(fips_codes, state_name, state_code, county_code),
                            by = c("county_fips" = "county_code", "state" = "state_name")) %>% 
  mutate(county_fips = paste0(state_code, county_fips)) %>% 
  select(-state_code)


voted_at_polls <- left_join(voted_at_polls, times, by = c("wait_cat" = "resp"))

voted_at_polls <- left_join(voted_at_polls, long, by = "wait_free") %>% 
  mutate(minutes = ifelse(is.na(time), minutes, time)) %>% 
  select(-time, -wait_free)

saveRDS(voted_at_polls, "./temp/waits_2008.rds")