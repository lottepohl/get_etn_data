# Script to download the dataset for the `ADST-Shark` project
# Libraries ####
install.packages("vctrs")

# install_version(
#   package,
#   version = NULL
library(vctrs)
library(etn)
library(dplyr)
library(lubridate)
library(utils)

# database connection ####

connection <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

# directory & setup ####
directory <- paste0(getwd(), "/ADST-Shark_acoustic")
animal_project_code <- "ADST-Shark"
scientific_name <- "Mustelus asterias"

# etn::download_acoustic_dataset(animal_project_code = "ADST-Shark", scientific_name = "Mustelus asterias")


## get animals ####

animals <- etn::get_animals(
  connection = connection,
  animal_project_code = animal_project_code,
  scientific_name = scientific_name)

readr::write_csv(animals, paste(directory, "animals.csv", sep = "/"), na = "")

## get tags ####

tag_serial_numbers <-
  animals %>%
  distinct(.data$tag_serial_number) %>%
  pull() %>%
  # To parse out multiple tags (e.g. "A69-9006-904,A69-9006-903"), combine
  # all tags and split them again on comma
  paste(collapse = ",") %>%
  strsplit("\\,") %>%
  unlist() %>%
  unique()

tags <- etn::get_tags(
  connection = connection,
  tag_serial_number = tag_serial_numbers
)

readr::write_csv(tags, paste(directory, "tags.csv", sep = "/"), na = "")

## get detections ####

detections <- get_acoustic_detections(
  connection = connection,
  animal_project_code = animal_project_code,
  scientific_name = scientific_name,
  limit = FALSE
)

# Keep unique records
detections_orig_count <- nrow(detections)
detections <-
  detections %>%
  distinct(.data$detection_id, .keep_all = TRUE)
readr::write_csv(detections, paste(directory, "detections.csv", sep = "/"), na = "")


## det deployments ####

acoustic_project_codes <-
  detections %>%
  distinct(.data$acoustic_project_code) %>%
  pull() %>%
  sort()

deployments <- get_acoustic_deployments(
  connection = connection,
  acoustic_project_code = acoustic_project_codes,
  open_only = FALSE
)

# Remove linebreaks in deployment comments to get single lines in csv:
deployments <-
  deployments %>%
  dplyr::mutate(comments = stringr::str_replace_all(.data$comments, "[\r\n]+", " "))

readr::write_csv(deployments, paste(directory, "deployments.csv", sep = "/"), na = "")


## get receivers ####

# Select on receivers associated with deployments
receiver_ids <-
  deployments %>%
  distinct(.data$receiver_id) %>%
  pull()
receivers <- get_acoustic_receivers(
  connection = connection,
  receiver_id = receiver_ids
)
readr::write_csv(receivers, paste(directory, "receivers.csv", sep = "/"), na = "")


## datapackage.JSON ####

datapackage <- system.file("assets", "datapackage.json", package = "etn")
file.copy(datapackage, paste(directory, "datapackage.json", sep = "/"), overwrite = TRUE)

