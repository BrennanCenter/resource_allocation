library(htmlwidgets)
library(ggiraph)
library(scales)
library(rgdal)
library(maptools)
library(readxl)
library(tidyverse)
library(data.table)


### start by reading number of poll workers, polling places, ages in each year

p16 <- fread("./raw_data/eavs/2016/EAVS 2016 Final Data for Public Release v.3.csv") %>% 
  select(FIPSCode, State, JurisdictionName,
         total_precincts = D1a,
         total_polling_places = D2a,
         ed_polling_places1 = D2b,
         ed_polling_places2 = D2c,
         ed_polling_places3 = D2d,
         early_polling_places1 = D2e,
         early_polling_places2 = D2f,
         early_polling_places3 = D2g,
         total_workers = D3a,
         workers_under_18 = D4a,
         workers_18_25 = D4b,
         workers_26_40 = D4c,
         workers_41_60 = D4d,
         workers_61_70 = D4e,
         workers_over_70 = D4f,
         difficulty_find_workers = D5,
         in_person_ballots_ed = F1b,
         in_person_ballots_early = F1f) %>% 
  mutate_at(vars(total_precincts, total_polling_places, total_workers,
                 starts_with("workers"), difficulty_find_workers, in_person_ballots_early,
                 in_person_ballots_ed,
                 starts_with("ed_polling"), starts_with("early_polling")),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888|666", .), "", .)))) %>% 
  mutate(early_vote_polling = ifelse(is.na(early_polling_places1), 0, early_polling_places1) +
           ifelse(is.na(early_polling_places2), 0, early_polling_places2) +
           ifelse(is.na(early_polling_places3), 0, early_polling_places3),
         ed_vote_polling = ifelse(is.na(ed_polling_places1), 0, ed_polling_places1) +
           ifelse(is.na(ed_polling_places2), 0, ed_polling_places2) +
           ifelse(is.na(ed_polling_places3), 0, ed_polling_places3),
         year = 2016,
         difficulty_find_workers = ifelse(difficulty_find_workers > 0 & difficulty_find_workers < 6,
                                          difficulty_find_workers, NA),
         FIPSCode = str_pad(as.character(FIPSCode), width = 10, side = "left", pad = "0")) %>% 
  select(-starts_with("ed_polling_places"), -starts_with("early_polling_places"))
##
p14a <- read_xlsx("./raw_data/eavs/2014/EAVS_Section_D.xlsx") %>% 
  select(FIPSCode, State, JurisdictionName = Jurisdiction,
         total_precincts = QD1a,
         total_polling_places = QD2a,
         ed_polling_places1 = QD2b,
         ed_polling_places2 = QD2c,
         ed_polling_places3 = QD2d,
         early_polling_places1 = QD2e,
         early_polling_places2 = QD2f,
         early_polling_places3 = QD2g,
         total_workers = QD3a,
         workers_under_18 = QD4a,
         workers_18_25 = QD4b,
         workers_26_40 = QD4c,
         workers_41_60 = QD4d,
         workers_61_70 = QD4e,
         workers_over_70 = QD4f,
         difficulty_find_workers = QD5) %>% 
  mutate(difficulty_find_workers = substring(difficulty_find_workers, 1, 1)) %>% 
  mutate_at(vars(total_precincts, total_polling_places, total_workers,
                 starts_with("workers"), difficulty_find_workers,
                 starts_with("ed_polling"), starts_with("early_polling")),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .)))) %>% 
  mutate(early_vote_polling = ifelse(is.na(early_polling_places1), 0, early_polling_places1) +
           ifelse(is.na(early_polling_places2), 0, early_polling_places2) +
           ifelse(is.na(early_polling_places3), 0, early_polling_places3),
         ed_vote_polling = ifelse(is.na(ed_polling_places1), 0, ed_polling_places1) +
           ifelse(is.na(ed_polling_places2), 0, ed_polling_places2) +
           ifelse(is.na(ed_polling_places3), 0, ed_polling_places3)) %>% 
  select(-starts_with("ed_polling_places"), -starts_with("early_polling_places"))

p14b <- read_xlsx("./raw_data/eavs/2014/EAVS_Section_F.xlsx") %>% 
  select(FIPSCode, State, JurisdictionName = Jurisdiction,
         in_person_ballots_ed = QF1b,
         in_person_ballots_early = QF1f) %>% 
  mutate_at(vars(in_person_ballots_ed,
                 in_person_ballots_early),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .))))

p14 <- full_join(p14a, p14b) %>% 
  mutate(year = 2014)
rm(p14a, p14b)
##
diff <- data.frame("difficulty_find_workers" = c("very difficult", "somewhat difficult", "neither difficult nor easy",
                                                 "somewhat easy", "very easy"),
                   "diff2" = c(1:5))
p12a <- read_xls("./raw_data/eavs/2012/Section D.xls") %>% 
  select(FIPSCode, State, JurisdictionName = Jurisdiction,
         total_precincts = QD1a,
         total_polling_places = QD2a,
         ed_polling_places1 = QD2b,
         ed_polling_places2 = QD2c,
         ed_polling_places3 = QD2d,
         early_polling_places1 = QD2e,
         early_polling_places2 = QD2f,
         early_polling_places3 = QD2g,
         total_workers = QD3a,
         workers_under_18 = QD4a,
         workers_18_25 = QD4b,
         workers_26_40 = QD4c,
         workers_41_60 = QD4d,
         workers_61_70 = QD4e,
         workers_over_70 = QD4f,
         difficulty_find_workers = QD5) %>% 
  mutate(difficulty_find_workers = tolower(difficulty_find_workers))

p12a <- left_join(p12a, diff, by = "difficulty_find_workers") %>% 
  mutate_at(vars(total_precincts, total_polling_places, total_workers,
                 starts_with("workers"), difficulty_find_workers,
                 starts_with("ed_polling"), starts_with("early_polling")),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .)))) %>% 
  mutate(early_vote_polling = ifelse(is.na(early_polling_places1), 0, early_polling_places1) +
           ifelse(is.na(early_polling_places2), 0, early_polling_places2) +
           ifelse(is.na(early_polling_places3), 0, early_polling_places3),
         ed_vote_polling = ifelse(is.na(ed_polling_places1), 0, ed_polling_places1) +
           ifelse(is.na(ed_polling_places2), 0, ed_polling_places2) +
           ifelse(is.na(ed_polling_places3), 0, ed_polling_places3),
         difficulty_find_workers = diff2) %>% 
  select(-starts_with("ed_polling_places"), -starts_with("early_polling_places"), -diff2)

p12b <- read_xls("./raw_data/eavs/2012/Section F.xls") %>% 
  select(FIPSCode = FIPSCode...3, State, JurisdictionName = Jurisdiction,
         in_person_ballots_ed = QF1b,
         in_person_ballots_early = QF1f) %>% 
  mutate_at(vars(in_person_ballots_ed,
                 in_person_ballots_early),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .))))

p12 <- full_join(p12a, p12b) %>% 
  mutate(year = 2012)
rm(p12a, p12b)
##
diff <- data.frame("difficulty_find_workers" = c("very difficult", "somewhat difficult", "neither",
                                                 "somewhat easy", "very easy"),
                   "diff2" = c(1:5))
p10a <- read_xlsx("./raw_data/eavs/2010/EAVS Section D.xlsx") %>% 
  select(FIPSCode, State, JurisdictionName = Jurisdiction,
         total_precincts = QD1a,
         total_polling_places = QD2a,
         ed_polling_places1 = QD2b,
         ed_polling_places2 = QD2c,
         ed_polling_places3 = QD2d,
         early_polling_places1 = QD2e,
         early_polling_places2 = QD2f,
         early_polling_places3 = QD2g,
         total_workers = QD3a,
         workers_under_18 = QD4a,
         workers_18_25 = QD4b,
         workers_26_40 = QD4c,
         workers_41_60 = QD4d,
         workers_61_70 = QD4e,
         workers_over_70 = QD4f,
         difficulty_find_workers = QD5) %>% 
  mutate(difficulty_find_workers = tolower(difficulty_find_workers))

p10a <- left_join(p10a, diff, by = "difficulty_find_workers") %>% 
  mutate_at(vars(total_precincts, total_polling_places, total_workers,
                 starts_with("workers"), difficulty_find_workers,
                 starts_with("ed_polling"), starts_with("early_polling")),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .)))) %>% 
  mutate(early_vote_polling = ifelse(is.na(early_polling_places1), 0, early_polling_places1) +
           ifelse(is.na(early_polling_places2), 0, early_polling_places2) +
           ifelse(is.na(early_polling_places3), 0, early_polling_places3),
         ed_vote_polling = ifelse(is.na(ed_polling_places1), 0, ed_polling_places1) +
           ifelse(is.na(ed_polling_places2), 0, ed_polling_places2) +
           ifelse(is.na(ed_polling_places3), 0, ed_polling_places3),
         difficulty_find_workers = diff2) %>% 
  select(-starts_with("ed_polling_places"), -starts_with("early_polling_places"), -diff2)

p10b <- read_xlsx("./raw_data/eavs/2010/EAVS Section F.xlsx") %>% 
  select(FIPSCode, State, JurisdictionName = Jurisdiction,
         in_person_ballots_ed = QF1b,
         in_person_ballots_early = QF1f) %>% 
  mutate_at(vars(in_person_ballots_ed,
                 in_person_ballots_early),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .))))

p10 <- full_join(p10a, p10b) %>% 
  mutate(year = 2010)
rm(p10a, p10b)
##
diff <- data.frame("difficulty_find_workers" = c("very difficult", "somewhat difficult", "neither difficult nor easy",
                                                 "somewhat easy", "very easy"),
                   "diff2" = c(1:5))
p08a <- read_xls("./raw_data/eavs/2008/Combined_SectionD.xls") %>% 
  select(FIPSCode = JurisID, State = STATE_, JurisdictionName = JurisName,
         total_precincts = D1,
         total_polling_places = D2a,
         ed_polling_places1 = D2b,
         ed_polling_places2 = D2c,
         ed_polling_places3 = D2d,
         early_polling_places1 = D2e,
         early_polling_places2 = D2f,
         early_polling_places3 = D2g,
         total_workers = D3,
         workers_under_18 = D4a,
         workers_18_25 = D4b,
         workers_26_40 = D4c,
         workers_41_60 = D4d,
         workers_61_70 = D4e,
         workers_over_70 = D4f,
         difficulty_find_workers = D5) %>% 
  mutate(difficulty_find_workers = tolower(difficulty_find_workers))

p08a <- left_join(p08a, diff, by = "difficulty_find_workers") %>% 
  mutate_at(vars(total_precincts, total_polling_places, total_workers,
                 starts_with("workers"), difficulty_find_workers,
                 starts_with("ed_polling"), starts_with("early_polling")),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .)))) %>% 
  mutate(early_vote_polling = ifelse(is.na(early_polling_places1), 0, early_polling_places1) +
           ifelse(is.na(early_polling_places2), 0, early_polling_places2) +
           ifelse(is.na(early_polling_places3), 0, early_polling_places3),
         ed_vote_polling = ifelse(is.na(ed_polling_places1), 0, ed_polling_places1) +
           ifelse(is.na(ed_polling_places2), 0, ed_polling_places2) +
           ifelse(is.na(ed_polling_places3), 0, ed_polling_places3),
         difficulty_find_workers = diff2) %>% 
  select(-starts_with("ed_polling_places"), -starts_with("early_polling_places"), -diff2)

p08b <- read_xls("./raw_data/eavs/2008/Combined_SectionF.xls") %>% 
  select(FIPSCode = JurisID, State = STATE_, JurisdictionName = JurisName,
         in_person_ballots_ed = F1b,
         in_person_ballots_early = F1f) %>% 
  mutate_at(vars(in_person_ballots_ed,
                 in_person_ballots_early),
            ~ as.numeric(trimws(ifelse(grepl(pattern = "999|888", .), "", .))))

p08 <- full_join(p08a, p08b) %>% 
  mutate(year = 2008)
rm(p08a, p08b)

#### combine

all_data <- rbind(p08, p10, p12, p14, p16) %>% 
  mutate(votes_per_worker = in_person_ballots_ed / total_workers)


saveRDS(all_data, "./temp/full_eavs.rds")
