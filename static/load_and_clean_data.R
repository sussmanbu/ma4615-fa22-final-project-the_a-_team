install.packages("here")
library(here)
library(tidyverse)
library(dplyr)

loan_data <- read_csv(here::here("dataset", "loan_refusal.csv"))

## CLEAN the data
loan_data_clean <- loan_data

write_csv(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.csv"))

save(loan_data_clean, file = here::here("dataset/loan_refusal.RData"))


## How we clean the datasets:


## check if there is problems in the dataset
air1 <- read_csv("dataset/AIRTRANS_CO2.csv", show_col_types = FALSE,
                 col_types = cols(
                   Time = col_character()
                 ))
problems(air1)

## filter out the years and country range we want
air1 <- air1 %>%  
  filter(LOCATION == "USA", values_drop_na = TRUE) %>% 
  filter(Time > 2009 & Time < 2020)

## rearrange years to make the trend clearer
## select out all columns we do not need
## rename columns to make them match among datasets
air1 <- air1 %>%
  arrange(Time) %>% 
  select(-POLLUTANT, -MEASURE, -FLIGHT, -FREQUENCY, -SOURCE, -SEASONALITY, -TIME, -`Flag Codes`, -Flags, -Pollutant, -LOCATION) %>%
  rename(flight_type = `Flight type`, source_of_emissions = `Source of emissions`, Year = Time, Emission = Value, `Element(tonnes)` = Measure) %>%
  rename(year = Year) %>%
  select(-Frequency, -Seasonality, -flight_type)

col_order <- c("Country", "year", "source_of_emissions",
                  "Emission", "Element(tonnes)")
air1 <- air1[, col_order]

air1
## store the data after we do all of data-cleaning
write_csv(air1, file = here::here("dataset", "air_clean.csv"))

## use kable to make dataset as table 
knitr::kable(head(air1[, 1:5]), caption = "The CO2 emissions in airlines in United States.", "simple")

#Load_data_clean
## Check if there is problems in the dataset
Agri <- read_csv("dataset/FAOSTAT.csv", show_col_types = FALSE)  
problems(Agri)

# CLEAN the data (second dataset)
## Filter out the country and emission element we want
Agri2 <- 
  Agri %>% filter(Area == "United States of America") %>%
  filter(str_detect(Element, "CO2eq")) %>%
  ## Select the years and the columns we need
  mutate(unit = "tonnes") %>%
  select(Area, Item, Element, unit, Y2010:Y2019) %>%
  pivot_longer(c(Y2010:Y2019),
               names_to = "year", values_to = "emission") %>%
  ## Delete unnecessary columns and arrange the data in a clear version           
  mutate(total = emission * 1000) %>%
  select(-emission) %>%
  arrange(order(year)) %>%
  group_by(year, Item, Area) %>%
  summarize(sum_emission = sum(total, na.rm = TRUE, count = n()), Element) %>%
  ## Rename the columns in order to match with other two datasets
  ## Do the final cleaning
  rename(Country = Area, Emission = sum_emission, source_of_emissions = Item, `Element(tonnes)` = Element) %>%
  mutate(year = gsub("Y", "", year)) %>%
  select(-Country)
Agri2
## Store the data after we do all of data-cleaning
write_csv(Agri2, file = here::here("dataset", "Agri_clean.csv"))

## Combine first two datasets
Agri_air <- rbind(Agri2, air1)
Agri_air
write_csv(Agri_air, file = here::here("dataset", "Agri_air_clean.csv"))

## third dataset:

## check if there is problems in the dataset:
vehi <- suppressMessages(read_csv("dataset/Vehicle.csv", show_col_types = FALSE, skip = 1,
                                  col_types = cols(
                                    `2010` = col_character(),
                                    `2014` = col_character(),
                                    `2011` = col_character(),
                                    `2012` = col_character(),
                                    `2013` = col_character()
                                  )))

problems(vehi)

## filter out some types of vehicles we do not focus on
## assign a new name to a column that didn't have a name
vehi1 <- vehi %>%
  filter(!row_number() %in% c(1,2,3,23,24,27:95)) %>%
  rename(source_of_emissions = ...1)

## select out the years we want to do research
## turn variable year as the column 
vehi1 <- vehi1 %>% 
  select(source_of_emissions, "2010":"2019") %>%
  pivot_longer(c("2010":"2019"), names_to = "year")

## remove all of NAs
## save the data after data-cleaning
vehi1 <- vehi1 %>% 
  na.omit(vehi) %>% 
  rename(type_of_cars = source_of_emissions)
write_csv(vehi1, file = here::here("dataset", "vehi_clean.csv"))

## Combine three datasets
Agri_air_car <- left_join(Agri_air, vehi1, by = "year")
Agri_air_car
knitr::kable(head(Agri2[, 1:5]), caption = "The CO2 emissions in combined airlines, agriculture, and vehicles in United States.", "simple")
write_csv(Agri_air_car, file = here::here("dataset", "Agri_air_clean.csv"))
