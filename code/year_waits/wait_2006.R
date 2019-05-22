

cces_2006 <- readstata13::read.dta13("./raw_data/cces/cces_2006_common.dta")

## set up conversion from answer to minutes
times <- data.frame("resp" = c("Not at all", "Less than 10 minutes", "10 to 30 minutes", "31 minutes to an hour"),
                    "minutes" = c(0, 5, 20, 45))

long <- fread("./raw_data/misc/free_wait_06.csv")


### pull people who voted at polls
voted_at_polls <- cces_2006 %>% 
  filter(v4006 == "In person on election day (at polling booth or precinct)",
         !is.na(v4006),
         v4009 != "Don't know") %>% 
  select(weight = v1001,
         wait_cat = v4009,
         wait_free = v4010,
         birthyr = v2020,
         gender = v2004,
         educ = v2018,
         race = v2005,
         marital = v2019,
         zip_code = v5047,
         county_fips = v1004,
         state = v1002,
         family_income = v2032) %>% 
  mutate(county_fips = str_pad(as.character(county_fips), width = 5, pad = "0", side = "left"))

voted_at_polls <- left_join(voted_at_polls, times, by = c("wait_cat" = "resp"))

voted_at_polls <- left_join(voted_at_polls, long, by = "wait_free") %>% 
  mutate(minutes = ifelse(is.na(time), minutes, time)) %>% 
  select(-time, -wait_free)

saveRDS(voted_at_polls, "./temp/waits_2006.rds")