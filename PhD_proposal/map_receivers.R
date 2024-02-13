

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
library(ggplot2)
library(ggspatial)
library(tidyr)

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
Europe <- mregions2::gaz_search(1920) %>% mregions2::gaz_geometry()


#### PLOT PBARN RECEIVERS ####

PBARN_acoustic_projects <- c() #c("ws1", "ws2", "cpodnetwork", "bpns", 'zeeschelde', "bovenschelde", "dijle", "Dijle_VPS", "Albertkanaal_VPS_Ham", "Albertkanaal_VPS_Hasselt", "albert", "	Walloneel")

# acoustic deployments

all_deployments <- 
  etn::get_acoustic_deployments(con, open_only = T) %>%
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
leaflet::leaflet() %>%
  addTiles() %>%
  addPolygons(data = BPNS, fillOpacity = 0, color = 'grey') %>%
  addCircleMarkers(data = new_receivers) %>%
  leafem::addMouseCoordinates()

  # addPolygons(data = Belgium, fillOpacity = 0, color = 'grey') %>%
  # addPolygons(data = BPNS, fillOpacity = 0, color = 'grey') %>%
  # addPolygons(data = Schelde_estuary, fillOpacity = 0, color = 'grey') %>%
  # addCircleMarkers(data = stations_PBARN)


#### GET ADDITIONAL SHAPEFILES ####

current_windfarms <- sf::st_read(paste0(getwd(),'/PhD_proposal/MUMM_Windfarm_Concessions_ETRS89.shp'))
power_cables <- sf::st_read(paste0(getwd(),'/PhD_proposal/MUMM_energy_cables_ETRS89_2.shp'))
MSP_1 <- sf::st_read(paste0(getwd(),'/PhD_proposal/rd20190522_art8_1_zone1.shp'))
MSP_2 <- sf::st_read(paste0(getwd(),'/PhD_proposal/rd20190522_art8_2_zone2.shp'))
MSP_3 <- sf::st_read(paste0(getwd(),'/PhD_proposal/rd20190522_art8_3_zone3.shp'))
MSP_4 <- sf::st_read(paste0(getwd(),'/PhD_proposal/rd20190522_art8_3_zone4.shp'))
energy_atoll_1 <- sf::st_read(paste0(getwd(),'/PhD_proposal/RD20140320_art8_5_zone1_20140407.shp'))
energy_atoll_2 <- sf::st_read(paste0(getwd(),'/PhD_proposal/RD20140320_art8_5_zone2_20140328.shp'))

#### SET ADDITIONAL RECEIVER COORDINATES ####

new_receivers <- tibble(
  names = c("latitude", 'longitude'),
  a1 = c(51.43, 3.15),
  a2 = c(51.43, 3.12),
  a3 = c(51.43, 3.09),
  a4 = c(51.46, 3.09),
  a5 = c(51.46, 3.11),
  a6 = c(51.46, 3.13),
  b1 = c(51.46, 3.03),
  b2 = c(51.46, 2.99),
  b3 = c(51.42, 3.00),
  b4 = c(51.42, 3.06),
  b5 = c(51.42, 2.95),
  b6 = c(51.49, 2.94),
  c1 = c(51.37, 2.40),
  c2 = c(51.38, 2.49),
  c3 = c(51.44, 2.46),
  c4 = c(51.42, 2.34),
  c5 = c(51.33, 2.39),
  c6 = c(51.35, 2.49),
  d1 = c(51.57, 2.44),
  d2 = c(51.62, 2.41),
  d3 = c(51.64, 2.48)
) %>%
  t() %>% as.data.frame() %>%
  dplyr::mutate(name = rownames(.)) %>%
  dplyr::rename(latitude = V1, longitude = V2) %>%
  dplyr::filter(!name == "names") %>%
  sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 3035) #4326
  

#### MAKE GGPLOT MAPS ####

europe_map <- ggplot() +
  geom_sf(data = Europe) +
  # coord_sf(crs = st_crs(4326)) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(35.5, 60), xlim = c(-14, 19)) +
  # coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(35, 60), xlim = c(-15, 15)) +
  geom_rect(mapping = aes(ymin = 50.5, ymax = 52, xmin = 1, xmax = 5), linewidth = 0.75, colour = "black", fill = "transparent") +
  theme_void() +  # Use a blank background for the main map
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.background = element_rect(color = NA, fill = "white"))

europe_map

europe_map_grob <- ggplot2::ggplotGrob(europe_map)


map_overview_ggplot <- ggplot() +
  # geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  geom_sf(data = UK, colour = "gray50") +
  geom_sf(data = Belgium, colour = "gray50") +
  geom_sf(data = France, colour = "gray50") +
  geom_sf(data = Netherlands, colour = "gray50") +
  geom_sf(data = current_windfarms, mapping = aes(colour = "OWF", fill = "OWF"), linewidth = 0.75) +
  # geom_sf(data = MSP_1, mapping = aes(colour = "MSP"), linewidth = 0.75) +
  geom_sf(data = MSP_2, mapping = aes(colour = "MSP", fill = "MSP"), linewidth = 0.75) +
  geom_sf(data = MSP_3, mapping = aes(colour = "MSP", fill = "MSP"), linewidth = 0.75) +
  geom_sf(data = MSP_4, mapping = aes(colour = "MSP", fill = "MSP"), linewidth = 0.75) +
  geom_sf(data = vlaamse_banken, mapping = aes(colour = "Natura2000"), fill = "transparent", linewidth = 0.75) + #, colour = "green", fill = "green"
  # geom_sf(data = energy_atoll_1, mapping = aes(colour = "MSP"), fill = "transparent", linewidth = 0.75) +
  # geom_sf(data = energy_atoll_2, mapping = aes(colour = "MSP"), fill = "transparent", linewidth = 0.75) +
  geom_sf(data = stations_PBARN, mapping = aes(colour = "PBARN"), fill = "red", linewidth = 0.15) +
  geom_sf(data = power_cables, mapping = aes(colour = "SPC"), linewidth = 0.75) +
  # geom_sf(data = new_receivers, colour = "red", linewidth = 0.75) +
  
  # marine boundaries
  geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  theme(panel.background = element_rect(fill = "#D9F6FA")) +
  labs(x = "Longitude", y = "Latitude") + #, tag ="b)"
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(51, 51.95), xlim = c(2, 4.5)) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.15, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    height =  unit(0.75, "cm"), width = unit(0.75, "cm"),
                                    pad_x = unit(0.075, "in"), pad_y = unit(0.15, "in"),
                                    style = north_arrow_fancy_orienteering) +
guides(
    colour = guide_legend(override.aes = list(shape = 19, size = 4)),
    fill = guide_legend(override.aes = list(shape = 0, size = 6, alpha = 0.5))) +
  annotation_custom(grob=europe_map_grob, xmin = 3.75, xmax = 4.65, ymin = 51, ymax=51.4) +
  
  scale_colour_manual(name = "", values = c("Natura2000" = "red", 
                                            "BPNS" = "darkorange",
                                             "OWF" = 'darkgreen',
                                             "SPC" = "darkblue",
                                            "MSP" = "lightgreen",
                                            "PBARN" = "black")) +
  scale_fill_manual(name = "", values = c(
    # "Natura2000" = "red", 
                                          # "BPNS" = "darkorange",
                                          "OWF" = 'darkgreen',
                                          # "SPC" = "darkblue",
                                          "MSP" = "lightgreen",
                                          "PBARN" = "black")) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal", legend.margin = margin(t = -5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2))

map_overview_ggplot 
ggplot2::ggsave(filename = paste0(getwd(),'/PhD_proposal/map.pdf'), plot = map_overview_ggplot, width = 16, height = 12, units = 'cm')
