

nys <- readRDS("./temp/new_york_race_census.RDS")

voted_general <- nys[nys$voted_general == T, ]

precincts <- full_join(
  voted_general %>% 
    group_by(election_district, assembly_district) %>% 
    summarize_at(vars(gender, dem, rep, yob, pred.whi, pred.bla, pred.his, pred.asi,
                      median_income, some_college, unem), funs(mean(., na = T))),
  voted_general %>% 
    group_by(election_district, assembly_district) %>% 
    tally(),
  by = c("assembly_district", "election_district")
)


precincts$ed <- as.integer(paste0(str_pad(precincts$assembly_district, width = 2, pad = "0", side = "left"),
                       str_pad(precincts$election_district, width = 3, pad = "0", side = "left")))


ed_shapefile <- readOGR("./raw_data/shapefiles/nyc_election_districts/nyed_19a", "nyed")
ed_shapefile@data$id <- rownames(ed_shapefile@data)
t <- fortify(ed_shapefile)
ed_shapefile <- inner_join(ed_shapefile@data, t, by = "id")


ed_map <- left_join(ed_shapefile, precincts, by = c("ElectDist" = "ed"))

ggplot() +
  geom_polygon(data = ed_map, aes(x = long, y = lat, group = group, fill = n)) +
  geom_path(data = ed_map, aes(x = long, y = lat, group = group), color = "white", size = 0.01) +
  coord_equal() + theme_map() +
  labs(fill = "Count of Voters by Precinct") +
  scale_fill_gradient(label = scales::comma, limits = c(299, 601), oob = scales::squish)

ggsave("./output/city_map_vcount.png")


ggplot() +
  geom_polygon(data = ed_map, aes(x = long, y = lat, group = group, fill = median_income)) +
  geom_path(data = ed_map, aes(x = long, y = lat, group = group), color = "white", size = 0.01) +
  coord_equal() + theme_map() +
  labs(fill = "Median Income by Precinct") +
  scale_fill_gradient(label = scales::dollar, limits = c(45000, 100000), oob = scales::squish)

ggsave("./output/city_map_income.png")


ggplot() +
  geom_polygon(data = ed_map, aes(x = long, y = lat, group = group, fill = pred.whi)) +
  geom_path(data = ed_map, aes(x = long, y = lat, group = group), color = "white", size = 0.01) +
  coord_equal() + theme_map() +
  labs(fill = "Share Non-Hispanic White by Precinct") +
  scale_fill_gradient(label = scales::percent, limits = c(0.15, 0.9), oob = scales::squish)

ggsave("./output/city_map_race.png")
