source("./code/misc/set_up_state_county_maps.R")
#### grab counties with biggest samples (ie, over 100)

load("./raw_data/cces/CCES2018_OUTPUT.RData")


times <- data.frame("resp" = c(1, 2, 3, 4, 5),
                    "quant" = c(0, 5, 20, 45, 60))


## county waits
wait_times_county <- left_join(table, times, by = c("CC18_404" = "resp")) %>% 
  mutate(quant = ifelse(CC18_403 == 3, 0, quant)) %>% 
  group_by(countyfips) %>% 
  summarize(wait_time = weighted.mean(quant, w = commonpostweight, na.rm = T),
            people_count = sum(commonpostweight, na.rm = T),
            n = n())


counties <- left_join(counties_maps, wait_times_county, by = c("GEOID" = "countyfips"))

counties$include <- !is.na(counties$wait_time) & counties$n >= 100


ggplot() +
  geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = wait_time), color = "black", size = 0.01) + 
  geom_path(data = states_maps, aes(x = long, y = lat, group = group)) +
  coord_equal() + theme_map() + scale_fill_gradient2(low = "#fee8c8", mid = "#fdbb84", high = "#f03b20", limits = c(0, 15)) +
  labs(fill = "Wait Time in Minutes", caption = "Source: CCES 2018\nNote: VMB assumed 0 minutes of waiting") + ggtitle("Wait Time in 2018 Election")
ggsave("./output/county_wait.png")

##state waits
wait_times_state <- left_join(table, times, by = c("CC18_404" = "resp")) %>% 
  mutate(quant = ifelse(CC18_403 == 3, 0, quant)) %>% 
  group_by(inputstate_post = str_pad(as.character(inputstate_post), width = 2, pad = "0", side = "left")) %>% 
  summarize(wait_time = weighted.mean(quant, w = commonpostweight, na.rm = T),
            people_count = sum(commonpostweight, na.rm = T),
            n = n())

states <- left_join(states_maps, wait_times_state, by = c("GEOID" = "inputstate_post"))

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group, fill = wait_time), color = "black", size = 0.01) + 
  coord_equal() + theme_map() + scale_fill_gradient2(low = "#fee8c8", mid = "#fdbb84", high = "#f03b20", limits = c(0, 15)) +
  labs(fill = "Wait Time in Minutes", caption = "Source: CCES 2018\nNote: VMB assumed 0 minutes of waiting") + ggtitle("Wait Time in 2018 Election")
ggsave("./output/state_wait.png")


we_have_vf <- states_maps %>% 
  filter(STUSPS %in% c("AK", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "MI",
                      "NV", "NJ", "NY", "NC", "OH", "OR", "PA", "RI", "TX", "VT",
                      "WA", "WI"))


ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group, fill = wait_time), color = "black", size = 0.01) + 
  geom_path(data = we_have_vf, aes(x = long, y = lat, group = group), color = "blue") +
  coord_equal() + theme_map() + scale_fill_gradient2(low = "#fee8c8", mid = "#fdbb84", high = "#f03b20", limits = c(0, 15)) +
  labs(fill = "Wait Time in Minutes", caption = "Source: CCES 2018\nNote: VMB assumed 0 minutes of waiting") + ggtitle("Wait Time in 2018 Election")
ggsave("./output/state_wait_we_have.png")


####

ggplot() + geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = include), color = "black", size = 0.01) + 
  geom_path(data = states_maps, aes(x = long, y = lat, group = group)) +
  coord_equal() + theme_map() + scale_fill_manual(values = c("gray", "red")) + theme(legend.position = "none") +
  labs(fill = NULL, caption = "Source: CCES 2018") +
  ggtitle("Counties with at least 100 Respondents in 2018 CCES")
ggsave("./output/counties_100.png", width = 7.5, height = 5)