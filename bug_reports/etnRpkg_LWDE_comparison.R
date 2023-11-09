
# load libraries
library(etn)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)

# 1. DATA FROM ETN R PACKAGE ####

## connect to db ####
con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))


## set parametersb ####
animal_project <- "2015_fint"
start <- "2022-01-01"
end <- "2022-01-31"


## query data ####
detections_shad <- etn::get_acoustic_detections(connection = con, 
                                                start_date = start,
                                                end_date = end,
                                                animal_project_code = animal_project)

## first overview ####
detections_shad_overview <- tibble::tibble(
  n_detections = detections_shad %>% nrow(),
  n_individuals = detections_shad$animal_id %>% unique() %>% length(),
  n_stations = detections_shad$station_name %>% unique() %>% length(),
  date_first_detection = detections_shad$date_time %>% min(),
  date_last_detection = detections_shad$date_time %>% max()
)

detections_shad_overview

detections_shad_summary <- detections_shad %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(n_detections = dplyr::n(),
                   deploy_latitude = deploy_latitude %>% mean(na.rm = T),
                   deploy_longitude = deploy_longitude %>% mean(na.rm = T))

detections_shad_summary

## overview map ####
leaflet(detections_shad_summary) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  leaflet.extras::addFullscreenControl() %>%
  addCircleMarkers(lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   radius = ~log(n_detections) + 2,
                   popup = ~paste0("<b>", "Station name: ", "</b>", station_name, "<br>",
                                   "<b>", "# detections: ", "</b>", n_detections))

## abacus plot ####
ggplot(detections_shad %>% 
         dplyr::mutate(animal_id = animal_id %>% as.character())) +
  geom_point(aes(x = date_time, y = animal_id, colour = station_name)) +
  theme_bw() 
