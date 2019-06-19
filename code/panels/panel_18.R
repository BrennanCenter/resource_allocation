source("./code/misc/AutoCluster4.R")

## zip
zips <- readRDS("./temp/zip_data.rds")

#### year matches - black voters

wait <- readRDS("./temp/waits_2018.rds") %>% 
  filter(race == 2)

income_lu <- data.frame("family_income" = c(1:16),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000, 90000, 110000, 135000, 150000, 150000, 150000, 150000, 150000))


wait <- left_join(wait, readRDS("./temp/covered.rds"), by = "county_fips")
wait <- left_join(wait, income_lu, by = "family_income")
wait$zip_code <- str_pad(as.character(wait$zip_code), pad = "0", side = "left", width = 5)

wait <- left_join(wait, zips, by = c("zip_code" = "zip"))

wait$marital <- ifelse(is.na(wait$marital), "NO ANSWER", wait$marital)
wait$income <- ifelse(is.na(wait$income), wait$median_income, wait$income)

wait <- wait[complete.cases(wait), ]

match_data <- wait %>% 
  dplyr::mutate(gender = gender == 2,
                college = educ %in% c(4:6),
                married = marital %in% c(1, 6),
                married_noans = marital == "NO ANSWER",
                dem = party == 1,
                rep = party == 2,
                indep = party == 3,
                early = vote_type == 2) %>% 
  dplyr::select(birthyr, income, gender, college, married, married_noans, dem, rep, indep, over65, share_english_very_well, pop_den, early)

cl <- NCPUS(detectCores() - 1)

genout <- GenMatch(Tr = wait$covered, X = match_data,
                   replace = T, pop.size = 1000, cluster = cl)

save(genout, file = "./temp/w18_genout.rdata")

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

saveRDS(matches, "./temp/balanced_18.rds")