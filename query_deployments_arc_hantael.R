# Script to query etn data for Arc'hantael

# Libraries ####

library(etn)
library(dplyr)
library(lubridate)
library(utils)

# database connection ####

con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))


# get deployments ####

deployments_arc <- etn::get_acoustic_deployments(con, open_only = FALSE) %>%
  dplyr::filter(deploy_date_time %>% lubridate::year() %>% dplyr::between(2019, 2022))

utils::write.csv(deployments_arc, file = paste0(getwd(), "/data/deployments_arc.csv"))



