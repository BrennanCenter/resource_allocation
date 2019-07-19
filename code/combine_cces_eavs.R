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

cces_16 <- readRDS("./temp/waits_2016.rds") %>% 
  mutate_if(is.factor, as.character) %>% 
  select(-wait_cat)

income <- fread("./raw_data/misc/income_lookup_16.csv")

cces_16 <- left_join(cces_16, income, by = c("family_income" = "code")) %>% 
  mutate(family_income = income,
         year = 2016) %>% 
  select(-income)

###
cces_18 <- readRDS("./temp/waits_2018.rds") %>% 
  select(-wait_cat)

race <- data.frame("race" = c(1:8),
                   "race2" = c("White", "Black", "Hispanic",
                               "Asian", "Native American", "Mixed",
                               "Other", "Middle Eastern"))
gender <- data.frame("gender" = c(1:2),
                     "gender2" = c("Male", "Female"))

educ <- data.frame("educ" = c(1:6),
                   "educ2" = c("No HS", "High school graduate",
                               "Some college", "2-year", "4-year",
                               "Post-grad"))

marital <- data.frame("marital" = c(1:6),
                      "marital2" = c("Married", "Separated",
                                     "Divorced", "Widowed",
                                     "Single", "Domestic partnership"))

income <- fread("./raw_data/misc/income_lookup.csv")

party <- data.frame("party" = c(1:4),
                    "party2" = c("Democrat", "Republican", "Independent", "Other"))

cces_18 <- left_join(cces_18, race, by = "race") %>% 
  select(-race) %>% 
  rename(race = race2)
cces_18 <- left_join(cces_18, gender, by = "gender") %>% 
  select(-gender) %>% 
  rename(gender = gender2)
cces_18 <- left_join(cces_18, educ, by = "educ") %>% 
  select(-educ) %>% 
  rename(educ = educ2)
cces_18 <- left_join(cces_18, marital, by = "marital") %>% 
  select(-marital) %>% 
  rename(marital = marital2)
cces_18 <- left_join(cces_18, income, by = c("family_income" = "code")) %>% 
  select(-family_income) %>% 
  rename(family_income = income)
cces_18 <- left_join(cces_18, party, by = "party") %>% 
  select(-party) %>% 
  rename(party = party2)
cces_18 <- cces_18 %>% 
  mutate(vote_type = ifelse(vote_type == 1, "In person on election day",
                            "In person before election day (early)"))
cces_18 <- left_join(
  cces_18,
  fips_codes %>% 
    mutate(state_code = as.integer(state_code)) %>% 
    select(state_code, state_name) %>% 
    group_by(state_code) %>% 
    filter(row_number() == 1) %>% 
    ungroup(),
  by = c("state" = "state_code")
) %>% 
  select(-state) %>% 
  rename(state = state_name) %>% 
  mutate(hispanic = ifelse(hispanic == 1, "Yes", "No"),
         year = 2018)
###
cces_16_18 <- bind_rows(cces_16, cces_18)

rm(educ, gender, income, marital, party, race, cces_16, cces_18)

full <- left_join(cces_16_18, eavs_data, by = c("county_fips", "year"))

full$vppp <- full$in_person_ballots_ed / full$total_polling_places

summary(lm(minutes ~ vppp + I(vppp^2) +
             relevel(as.factor(race), ref = "White") +
             as.factor(party) + family_income + as.factor(year) +
             as.factor(state),
           data = filter(full, in_person_ballots_ed > 0, total_polling_places > 0), weights = weight))

summary(lm(minutes ~ as.factor(difficulty_find_workers) +
             relevel(as.factor(race), ref = "White") +
             as.factor(party) + family_income + as.factor(year) +
             as.factor(state),
           data = filter(full, !is.infinite(difficulty_find_workers)), weights = weight))

full$long_wait <- full$minutes > 30

summary(glm(long_wait ~ as.factor(difficulty_find_workers) +
             relevel(as.factor(race), ref = "White") +
             as.factor(party) + family_income + as.factor(year) +
             as.factor(state),
           data = filter(full, !is.infinite(difficulty_find_workers)), weights = weight, family = "binomial"))
