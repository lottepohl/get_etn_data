

#### WORKSPACE ####

library(etn)
library(dplyr) # for data wrangling
library(lubridate) # for timeanddate operations
library(crosstalk) # for on-the-fly data filtering
library(utils) # for miscellaneous operation
library(htmltools) # for html components
library(leaflet)# for interactive maps
library(knitr) # for knitting the document into a e.g. .html file
library(leafem) # for extras in leaflet maps
library(DT) # for interactive tables
library(mregions2) # to query marine geospatial boundaries

# connect to ETN
con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

# get marine regions boundaries
BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry()
vlaamse_banken <- mregions2::gaz_search(29402) %>% mregions2::gaz_geometry()
Belgium <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()
Netherlands <- mregions2::gaz_search(15) %>% mregions2::gaz_geometry()
France <- mregions2::gaz_search(17) %>% mregions2::gaz_geometry()
Germany <- mregions2::gaz_search(2101) %>% mregions2::gaz_geometry()
UK <- mregions2::gaz_search(2208) %>% mregions2::gaz_geometry()
Southern_North_Sea <- mregions2::gaz_search(22253) %>% mregions2::gaz_geometry()
Schelde_estuary <- mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()


#### PLOT PBARN RECEIVERS ####

PBARN_acoustic_projects <- c("ws1", "ws2", "cpodnetwork", "bpns", 'zeeschelde', "bovenschelde", "dijle", "Dijle_VPS", "Albertkanaal_VPS_Ham", "Albertkanaal_VPS_Hasselt", "albert", "	Walloneel")

# acoustic deployments

all_deployments <- 
  etn::get_acoustic_deployments(con) %>%
  dplyr::filter(!is.na(deploy_longitude) & !is.na(deploy_latitude)) %>%
  sf::st_as_sf(., coords = c("deploy_longitude", "deploy_latitude"), crs = 4326)


within_BPNS <- sf::st_within(all_deployments, BPNS) %>% # get back list with TRUE or FALSE if deployment location is within BPNS boundaries
  apply(., 1, any)
within_Belgium <-  sf::st_within(all_deployments, Belgium) %>% # get back list with TRUE or FALSE if deployment location is within Belgium's boundaries
  apply(., 1, any)
within_Schelde_estuary <- sf::st_within(all_deployments, Schelde_estuary) %>% # get back list with TRUE or FALSE if deployment location is within the Scheldt estuary's boundaries
  apply(., 1, any) 


deployments_PBARN <- all_deployments %>%
  dplyr::mutate(within_BPNS = within_BPNS,
                within_Belgium = within_Belgium) %>%
  dplyr::filter(within_BPNS == TRUE | within_BPNS == TRUE | within_Schelde_estuary == TRUE| acoustic_project_code %in% PBARN_acoustic_projects) # filter out stations that are not in any of the boundaries defined

stations_PBARN <- 
  deployments_PBARN %>% 
  dplyr::select(station_name) %>%
  unique() # get the station names

# # quick map to see the distribution of stations
# leaflet::leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = Belgium, fillOpacity = 0, color = 'grey') %>%
#   addPolygons(data = BPNS, fillOpacity = 0, color = 'grey') %>%
#   addPolygons(data = Schelde_estuary, fillOpacity = 0, color = 'grey') %>%
#   addCircleMarkers(data = stations_PBARN)


#### GET ADDITIONAL SHAPEFILES ####



