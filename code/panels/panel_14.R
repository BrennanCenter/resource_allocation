source("./code/misc/AutoCluster4.R")

## zip
zips <- readRDS("./temp/zip_data.rds")

#### year matches - black voters

wait <- readRDS("./temp/waits_2014.rds") %>% 
  filter(race == "Black")

income_lu <- data.frame("family_income" = c("Less than $10,000", "$10,000 - $19,999",
                                            "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999",
                                            "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999",
                                            "$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999",
                                            "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999",
                                            "$250,000 or more", "$350,000 - $499,999", "$500,000 or more"),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000, 90000, 110000,
                                     135000, 150000, 150000, 150000, 150000, 150000, 150000))


wait <- left_join(wait, readRDS("./temp/covered.rds"), by = "county_fips")
wait <- left_join(wait, income_lu, by = "family_income")
wait$zip_code <- str_pad(as.character(wait$zip_code), pad = "0", side = "left", width = 5)

wait <- left_join(wait, zips, by = c("zip_code" = "zip"))

wait$marital <- ifelse(is.na(wait$marital), "NO ANSWER", wait$marital)
wait$income <- ifelse(is.na(wait$income), wait$median_income, wait$income)

wait <- wait[complete.cases(wait), ]

match_data <- wait %>% 
  dplyr::mutate(gender = gender == "Female",
                college = educ %in% c("2-year", "4-year", "Post-grad"),
                married = marital %in% c("Married", "Domestic partnership"),
                married_noans = marital == "NO ANSWER",
                dem = party == "Democrat",
                rep = party == "Republican",
                indep = party == "Independent",
                early = vote_type == "In person before election day (early)") %>% 
  dplyr::select(birthyr, income, gender, college, married, married_noans, dem, rep, indep, over65, share_english_very_well, pop_den, early)

cl <- NCPUS(detectCores() - 1)

genout <- GenMatch(Tr = wait$covered, X = match_data,
                   replace = T, pop.size = 1000, cluster = cl)

save(genout, file = "./temp/w14_genout.rdata")

load("./temp/w14_genout.rdata")

mout <- Match(Tr = wait$covered, X = match_data, estimand = "ATT", Weight.matrix = genout, M = 10)

matches1 <- data.frame("id" = mout[["index.control"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(match_weight = sum(weight)) %>% 
  mutate(treat = F)

matches2 <- data.frame("id" = mout[["index.treated"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(match_weight = sum(weight)) %>% 
  mutate(treat = T)

matches <- bind_rows(matches1, matches2)

wait <- wait %>% 
  mutate(id = row_number())

matches <- left_join(matches, wait, by = "id")

saveRDS(matches, "./temp/balanced_14.rds")