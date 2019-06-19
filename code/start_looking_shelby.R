
all <- rbindlist(lapply(c(2006, 2008, 2012, 2014, 2016, 2018), function(year){
  j <- readRDS(paste0("./temp/waits_", year, ".rds")) %>% 
    mutate(year = year) %>% 
    filter(!is.na(weight),
           race != 1,
           race != "White")
  
}), fill = T)



ll <- rbindlist(lapply(c(2006, 2008, 2012, 2014, 2016, 2018), function(year){
  j <- readRDS(paste0("./temp/waits_", year, ".rds")) %>% 
    mutate(year = year,
           black = (race == 2 | race == "Black")) %>% 
    filter(!is.na(weight))
  
  covered <- readRDS("./temp/covered.rds")
  j <- left_join(j, covered, by = "county_fips")
  
  j <- j %>% 
    group_by(year, covered, black) %>% 
    summarize(wait = sum(minutes * weight, na.rm = T) / sum(weight, na.rm = T))
  
}), fill = T)


ll <- filter(ll, !is.na(covered))




ggplot(filter(ll, black == F), aes(x = year, y = wait, color = covered)) + geom_line()