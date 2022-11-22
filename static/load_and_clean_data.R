library(tidyverse)

Agri_air_car <- read_csv(here::here("dataset", "Agri_air_car.csv"))

## CLEAN the data
Agri_air_car_clean <- Agri_air_car

write_csv(Agri_air_car_clean, file = here::here("dataset", "Agri_air_car_clean.csv"))

save(Agri_air_car_clean, file = here::here("dataset/Agri_air_car.RData"))
