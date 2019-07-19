library(scales)
library(Matching)
library(tidycensus)
library(rgeos)
library(wru)
library(sqldf)
library(extrafont)
library(maptools)
library(mapproj)
library(rgdal)
library(data.table)
library(tidyverse)
library(kevostools)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1.3, "cm"),
      plot.title = element_text(family = "Gadugi", color = "black", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(family = "Gadugi", color = "#525353"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      plot.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

source("./code/misc/get_basic_census.R")

db <- dbConnect(SQLite(), "D:/rolls.db")
api_key <- Sys.getenv("CENSUS_API_KEY")

