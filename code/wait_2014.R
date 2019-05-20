

cces_2014 <- readstata13::read.dta13("./raw_data/cces/CCES14_Common_Content_Validated.dta")

## set up conversion from answer to minutes
times <- data.frame("resp" = c("Not at all", "Less than 10 minutes", "10 - 30 minutes", "31 minutes - 1 hour"),
                    "minutes" = c(0, 5, 20, 45))

long <- fread("./raw_data/misc/free_wait_14.csv")


### pull people who voted at polls
voted_at_polls <- cces_2014 %>% 
  filter(CC403 == "In person on election day",
         !is.na(CC403),
         CC404 != "Don't know") %>% 
  select(weight,
         wait_cat = CC404,
         wait_free = CC404_t,
         birthyr,
         gender,
         educ,
         race,
         race_other,
         hispanic,
         marital = marstat,
         zip_code = lookupzip,
         county_fips = countyfips,
         state = inputstate_post,
         family_income = faminc)


voted_at_polls <- left_join(voted_at_polls, times, by = c("wait_cat" = "resp"))

voted_at_polls <- left_join(voted_at_polls, long, by = "wait_free") %>% 
  mutate(minutes = ifelse(is.na(time), minutes, time)) %>% 
  select(-time, -wait_free)

saveRDS(voted_at_polls, "./temp/waits_2014.rds")