house_results <- fread("./raw_data/results/district_overall_2018.csv") %>% 
  group_by(state_fips, district, candidate) %>% 
  filter(row_number() == 1) %>% 
  group_by(state_fips, district) %>% 
  mutate(totalvotes = sum(candidatevotes)) %>% 
  arrange(desc(candidatevotes)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(share = candidatevotes / totalvotes,
         district = gsub("District ", "", district)) %>% 
  select(share, state = state_fips, district)

gubernatorial <- fread("./raw_data/results/state_overall_2018.csv") %>% 
  filter(office %in% c("Governor", "Governor & Lt Governor", "Governor / Lt. Governor",
                       "Governor and Lieutenant Governor", "Governor and Lt. Governor",
                       "Governor/Lieutenant Governor", "Governor/Lt. Governor")) %>% 
  group_by(state_fips) %>% 
  filter(row_number() == 1) %>% 
  select(state = state_fips)

senate <- fread("./raw_data/results/senate_overall_2018.csv") %>% 
  group_by(state_fips) %>% 
  filter(row_number() == 1) %>% 
  select(state = state_fips)

cces_2016 <- readstata13::read.dta13("./raw_data/cces/CCES16_Common_OUTPUT_Feb2018_VV.dta") %>% 
  mutate(CC16_401 = ifelse(is.na(CC16_401), "", CC16_401),
         voted = CC16_401 == 5) %>% 
  group_by(countyfips_post) %>% 
  summarize(to16 = weighted.mean(voted, commonweight_vv_post))

w16 <- readRDS("./temp/waits_2016.rds") %>% 
  group_by(county_fips) %>% 
  summarize(n = n(),
            wait_16 = weighted.mean(minutes, weight)) %>% 
  filter(n >= 50)

cces_2018 <- readstata13::read.dta13("./raw_data/cces/CCES2018_OUTPUT.dta")

cces_2018 <- inner_join(cces_2018, w16, by = c("countyfips" = "county_fips"))

cces_2018$gov <- cces_2018$inputstate %in% gubernatorial$state
cces_2018$senate <- cces_2018$inputstate %in% senate$state

cces_2018 <- left_join(cces_2018, house_results, by = c("inputstate" = "state", "cdid116" = "district"))
cces_2018 <- left_join(cces_2018, cces_2016, by = c("countyfips" = "countyfips_post"))

cces_2018$voted <- cces_2018$CC18_401 == 5


summary(glm(voted ~ wait_16 + gov + senate + share + as.factor(race) +
              faminc_new + gender_post + to16, data = cces_2018, family = "binomial", weights = commonpostweight))