library(etn)
library(dplyr)
library(mregions2)
library(utils)
library(mapview)

con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

etn_species <- etn::list_scientific_names()

etn_elasmo <- c("Galeorhinus galeus","Lamna nasus", "Prionace glauca", "Raja undulata", "Scyliorhinus canicula", "Torpedo torpedo", "Hexanchus griseus", "Scyliorhinus stellaris",
                "Squalus acanthias", "Cetorhinus maximus", "Raja asterias", "Squatina squatina", "Raja brachyura", "Sphyrna zygaena", "Dalatias licha", "Isurus oxyrinchus", "Mustelus asterias", 
                "Raja clavata", "Rhynchobatus djiddensis", "Carcharhinus albimarginatus", "Dasyatis pastinaca", "Galeocerdo cuvier", "Mustelus sp.", "Raja radula", "Rostroraja alba", "Torpedo marmorata"
                )

detections_elasmo <- etn::get_acoustic_detections(con, scientific_name = etn_elasmo)

North_Sea <- mregions2::gaz_search(2350)

det_northsea_elasmo <- detections_elasmo %>%
  dplyr::filter(deploy_latitude %>% between(North_Sea$minLatitude, North_Sea$maxLatitude),
                deploy_longitude %>% between(North_Sea$minLongitude, North_Sea$maxLongitude),
                !scientific_name == "Galeocerdo cuvier"
                )

det_northsea_elasmo$scientific_name %>% unique()

det_summary_northsea_elasmo <- det_northsea_elasmo %>%
  dplyr::group_by(scientific_name, station_name) %>%
  dplyr::summarise(n_detections = dplyr::n(),
                   first_detection = date_time %>% min(),
                   last_detection = date_time %>% max(),
                   deploy_latitude = deploy_latitude %>% mean(na.rm = T),
                   deploy_longitude = deploy_longitude %>% mean(na.rm = T))


m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = det_summary_northsea_elasmo,
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   radius = ~log(n_detections),
                   label = ~paste0(scientific_name, ", n_detect = ", n_detections))
m
