

cces_2018 <- readstata13::read.dta13("./raw_data/cces/CCES2018_OUTPUT.dta")

## set up conversion from answer to minutes
times <- data.frame("resp" = c(1, 2, 3, 4),
                    "minutes" = c(0, 5, 20, 45))

long <- fread("./raw_data/misc/free_wait_18.csv")


### pull people who voted at polls
voted_at_polls <- cces_2018 %>% 
  filter(CC18_403 == 1,
         !is.na(CC18_403),
         CC18_404 != 6) %>% 
  select(weight = commonpostweight,
         wait_cat = CC18_404,
         wait_free = CC18_404_t,
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
         family_income = faminc_new)


voted_at_polls <- left_join(voted_at_polls, times, by = c("wait_cat" = "resp"))

voted_at_polls <- left_join(voted_at_polls, long, by = "wait_free") %>% 
  mutate(minutes = ifelse(is.na(time), minutes, time)) %>% 
  select(-time, -wait_free)

saveRDS(voted_at_polls, "./temp/waits_2018.rds")