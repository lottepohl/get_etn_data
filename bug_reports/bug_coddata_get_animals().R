# Script to demonstrate that some seabass animals cannot be retrieved via the etn R package

# libraries
library(etn)
library(dplyr)

# database connection
con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

bass <- "Dicentrarchus labrax"

bass_animal_ids <- c(65557, 65555, 65556, 23107, 23104, 65558, 65553, 65552, 65554)
bass_acoustic_tag_ids <- c("OPS-224","OPS-248", "OPS-244", "OPS-262",
                           "OPS-200", "OPS-208", "OPS-198", "OPS-234",
                           "OPS-268", "OPS-228", "OPS-247", "OPS-250")

# get detections to get animal_id (which is used as input for `etn::get_animals()`)
detections_bass <- etn::get_acoustic_detections(con, acoustic_tag_id = bass_acoustic_tag_ids)
detected_bass <- detections_bass %>%
  dplyr::group_by(animal_id) %>%
  dplyr::summarise(tag_serial_number = tag_serial_number %>% unique(),
                   acoustic_tag_id = acoustic_tag_id %>% unique(),
                   n_detections = n(),
                   first_detect = min(date_time),
                   last_detect = max(date_time))

# retrieve animal data
animals_bass <- etn::get_animals(animal_id = detected_bass$animal_id)

# no animals found
nrow(animals_bass)
