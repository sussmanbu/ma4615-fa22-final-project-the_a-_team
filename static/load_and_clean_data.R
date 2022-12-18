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

## first dataset: total greenhouse emissions in differnet sectors
## check if there is problems in the dataset
library(here)
library(tidyverse)
library(dplyr)
co2_data <- read_csv("dataset/owid-co2-data.csv", show_col_types = FALSE)


co2_data1 <- co2_data %>%
  ## select variables we need to do modeling
  select(country, year, population, gdp, energy_per_capita, energy_per_gdp, primary_energy_consumption, total_ghg) %>%
  ## omit NAs
  na.omit() 

co2_data1
## save the cleaned dataset
write_csv(co2_data1, file = here::here("dataset", "co2_data1_clean.csv"))
save(co2_data1, file = here::here("dataset/co2_data1_clean.RData"))


## second dataset: agriculture emissions
Agri <- read_csv("dataset/FAOSTAT.csv", show_col_types = FALSE) 

Agri_land <- 
  Agri %>%
  ## find out items related to agriculture
  filter(str_detect(Item,"Emissions on agricultural land")) %>%
  ## sort out emissions related to CO2 emissions
  filter(str_detect(Element, "CO2eq")) %>%
  ## make sure the unit is same for both datasets
  mutate(unit = "tonnes") %>%
  ## select out columns we need
  select(Area, Item, Element, unit, Y1990:Y2018) %>%
  ## make the variable year to one column
  pivot_longer(c(Y1990:Y2018),
               names_to = "year", values_to = "emission") %>%
  ## create a new column with new emissions
  mutate(total = emission * 1000) %>%
  ## delete the unnecessary column
  select(-emission) %>%
  ## arrange by year
  arrange(order(year)) %>%
  ## group by variables
  group_by(year, Item, Area) %>%
  ## calculate the total emissions by the group
  summarize(sum_emission = sum(total, na.rm = TRUE, count = n()), Element) %>%
  ## rename all of columns for combining
  rename(Country = Area, Emission = sum_emission, source_of_emissions = Item, `Element(tonnes)` = Element) %>%
  ## remove 'Y' in column year
  mutate(year = gsub("Y", "", year)) %>%
  select(-Country) %>%
  ungroup() %>%
  select(-`Element(tonnes)`)%>%
  ## calculate total emissions
  group_by(Country, year, source_of_emissions) %>%
  summarise(Emissions = sum(Emission)) %>%
  rename(country = Country) %>%
  ## make sure names of countries are same to avoid NAs
  mutate(country = gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", country)) %>%
  mutate(country = gsub("United States of America", "United States", country)) %>%
  mutate(country = gsub("Russian Federation", "Russia", country)) %>%
  mutate(country = gsub("Bolivia (Plurinational State of)", "Bolivia", country)) %>%
  mutate(country = gsub("Republic of Moldova", "Moldova", country)) %>%
  mutate(country = gsub("Lao People's Democratic Republic", "Laos", country)) %>%
  mutate(country = gsub("Syrian Arab Republic", "Syria", country)) %>%
  mutate(country = gsub("United Republic of Tanzania", "Tanzania", country)) %>%
  mutate(country = gsub("Democratic Republic of the Congo", "Democratic Republic of Congo", country)) %>%
  mutate(country = gsub("Iran (Islamic Republic of)", "Iran", country))
Agri_land$year = as.numeric(Agri_land$year)
Agri_land
## save the cleaned dataset
write_csv(Agri_land, file = here::here("dataset", "Agri_land_clean.csv"))
save(Agri_land, file = here::here("dataset/Agri_land_clean.RData"))

## combine two datasets

co2_agri <- full_join(co2_data1, Agri_land, by = c("year", "country"))

co2_agri <- co2_agri %>% 
  ## make sure the unit is identical
  mutate(total_ghg2 = total_ghg * 1000) %>%
  select(-total_ghg) %>%
  filter(country != "World") %>%
  ## make sure the unit is identical
  rename(`Agriculture_emissions(kilotonnes)` = Emissions, `total_ghg(kilotonnes)` = total_ghg2) %>%
  mutate(`Agriculture_emissions(Kilotonnes)`= `Agriculture_emissions(kilotonnes)`/1000) %>%
  select(-`Agriculture_emissions(kilotonnes)`)
  ## omit NAs are omitted
co2_agri <- co2_agri %>% mutate(agri_ghg_total = rowSums(co2_agri[,c(9,10)])) %>%
  na.omit()

co2_agri

write_csv(co2_agri, file = here::here("dataset", "co2_agri_clean.csv"))
save(co2_agri, file = here::here("dataset/co2_agri_clean.RData"))
