# Script to select acoustic receiver stations with PBARN (i.e., the BPNS, Belgium and the Schelde estuary) and to make an overview of detected species per institution
# Author: Lotte Pohl (lotte.pohl@vliz.be)
# Date: 2024-02-02

#### 1. SETUP WORKSPACE ####

# Libraries 
library(flexdashboard)
# library(devtools)
# devtools::install_github("inbo/etn")
library(etn) # to retrieve data from the etn database
library(dplyr) # for data wrangling
library(tidyr) # to filter out NAs
library(leaflet)# for interactive maps
library(writexl) # for making excel tables
library(sf) # for geospatial operations
library(mregions2) # to query marine geospatial boundaries

# database connection
con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

# boundaries of the belgian part of the north sea (BPNS), Belgium, and the Schelde estuary
BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry()
Belgium <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()
Schelde_estuary <- mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()

# Additional acoustic project codes within the PBARN (just as a backup to make sure no stations are missed)
PBARN_acoustic_projects <- c("ws1", "ws2", "cpodnetwork", "bpns", 'zeeschelde', "bovenschelde")


#### 2. GET PBARN RECEIVER STATIONS ####

# query acoustic deployments
all_deployments <- 
  etn::get_acoustic_deployments(con) %>%
  dplyr::filter(!is.na(deploy_longitude) & !is.na(deploy_latitude)) %>%
  sf::st_as_sf(., coords = c("deploy_longitude", "deploy_latitude"), crs = 4326)

# assess which deployments lie inside the geospatial boundaries of the BPNS, Belgium and the Schelde estuary
within_BPNS <- sf::st_within(all_deployments, BPNS) %>% # get back list with TRUE or FALSE if deployment location is within BPNS boundaries
  apply(., 1, any)
within_Belgium <-  sf::st_within(all_deployments, Belgium) %>% # get back list with TRUE or FALSE if deployment location is within Belgium's boundaries
  apply(., 1, any)
within_Schelde_estuary <- sf::st_within(all_deployments, Schelde_estuary) %>% # get back list with TRUE or FALSE if deployment location is within the Scheldt estuary's boundaries
  apply(., 1, any) 

# filter out deployments outside the defined geospatial boundaries
deployments_PBARN <- all_deployments %>%
  dplyr::mutate(within_BPNS = within_BPNS,
                within_Belgium = within_Belgium) %>%
  dplyr::filter(within_BPNS == TRUE | within_BPNS == TRUE | within_Schelde_estuary == TRUE| acoustic_project_code %in% PBARN_acoustic_projects) # filter out stations that are not in any of the boundaries defined

# get the station names of the deployments -> this will be our list of PBARN stations
stations_PBARN <- 
  deployments_PBARN %>% 
  dplyr::select(station_name) %>%
  unique() 

# quick map to see the distribution of stations -> in the east of Belgium there are still some stations missing but I guess for us marine people it doesn't matter that much...
leaflet::leaflet() %>%
  addTiles() %>%
  addPolygons(data = Belgium, fillOpacity = 0, color = 'grey') %>%
  addPolygons(data = BPNS, fillOpacity = 0, color = 'grey') %>%
  addPolygons(data = Schelde_estuary, fillOpacity = 0, color = 'grey') %>%
  addCircleMarkers(data = stations_PBARN)

#### 3. SUMMARISE ACOUSTIC DETECTIONS OF 2023 ####

# query detections from the list of stations within PBARN
detections_2023 <- etn::get_acoustic_detections(con, start_date = "2023-01-01", end_date = "2023-12-31", station_name = stations_PBARN$station_name)

# select animals of the acoustic detections
animals_detected <- 
  detections_2023 %>%
  dplyr::select(tag_serial_number) %>% 
  unique() %>%
  tidyr::drop_na() # filter out NAs

# query more information on the animals
animals_2023 <- etn::get_animals(con,
                                 tag_serial_number = animals_detected$tag_serial_number)

# query information on the tags (here is the field 'owner organisation') which corresponds to the institute that tagged the fish 
tags_2023 <- 
  etn::get_tags(con,
                tag_serial_number = animals_detected$tag_serial_number)

# join all queried info together
detections_institutes_2023 <- 
  detections_2023 %>%
  dplyr::select(detection_id, date_time, tag_serial_number, acoustic_tag_id, animal_project_code, animal_id,
                scientific_name, acoustic_project_code, station_name, deploy_latitude, deploy_longitude, deployment_id) %>%
  dplyr::left_join(animals_2023 %>%
                     dplyr::select(tag_serial_number, tagger, release_date_time, release_location, release_latitude,
                                   release_longitude, length1, length1_unit, weight, weight_unit, sex, comments),
                   by = join_by(tag_serial_number),
                   relationship = "many-to-many") %>%
  dplyr::left_join(tags_2023 %>%
                     dplyr::select(tag_serial_number, owner_organization, owner_pi),
                   by = join_by(tag_serial_number),
                   relationship ="many-to-many")


# SUMMARY PER DETECTED SPECIES
dets_inst_summary <- 
  detections_institutes_2023 %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(n_detections = detection_id %>% unique() %>% length(),
                   n_individuals = tag_serial_number %>% unique() %>% length(),
                   n_animal_projects = animal_project_code %>% unique() %>% length(),
                   n_owner_organizations = owner_organization %>% unique() %>% length(),
                   n_stations = station_name %>% unique() %>% length(),
                   animal_projects = paste(animal_project_code %>% unique(), collapse = ", "),
                   owner_organizations = paste(owner_organization %>% unique(), collapse = ", "),
                   stations = paste(station_name %>% unique(), collapse = ", "))

# dets_inst_summary %>% View()
print(dets_inst_summary, n = nrow(dets_inst_summary))


# SUMMARY PER STATION WITH DETECTIONS
stations_inst_summary <- 
  detections_institutes_2023 %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(n_detections = detection_id %>% unique() %>% length(),
                   n_species = scientific_name %>% unique() %>% length(),
                   n_individuals = tag_serial_number %>% unique() %>% length(),
                   n_animal_projects = animal_project_code %>% unique() %>% length(),
                   n_owner_organizations = owner_organization %>% unique() %>% length(),
                   animal_projects = paste(animal_project_code %>% unique(), collapse = ", "),
                   owner_organizations = paste(owner_organization %>% unique(), collapse = ", "),
                   species = paste(scientific_name %>% unique(), collapse = ", "))

# stations_inst_summary %>% View()
print(stations_inst_summary, n = nrow(stations_inst_summary))


# MAKE OUTPUT
writexl::write_xlsx(dets_inst_summary, path = paste0(getwd(), "/20240129_PBARN_detections_overview/2021-2023_PBARN_species_institutes_overview.xlsx"))
writexl::write_xlsx(stations_inst_summary, path = paste0(getwd(), "/20240129_PBARN_detections_overview/2021-2023_PBARN_stations_institutes_overview.xlsx"))

