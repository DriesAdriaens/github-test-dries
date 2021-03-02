
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(leaflet)
library(foreign)
library(googlesheets4)
library(readxl)

test <- read_xlsx(path = "G:/Mijn Drive/Projecten/PRJ_SBO_Future_Floodplains/140_RawData/Stavaza/Overzicht opnames en metadata 2018-2019.xlsx")

test <- test %>% 
  mutate(across(starts_with(c("OpnX","OpnY", "OpnZ", "Peilbuis_filterdiepte", "GXG", "Afst_peilb",
                              "check_FLAVEN", "Grondwaterstaal_datum", "Peilbuis_", "Peilbuisafstand", "Z_diff_"))|
                  contains("pHAB"), as.numeric))


test2 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1TsinQR-yJOl6n1QB4RwaNbCsGbOct1PTHMdNenl2Ef4/edit?usp=sharing",
                    range = "Deelzones_hydroherstel!A3:AV224", guess_max = 50)
test2 <- test2 %>% 
  mutate(across(where(is.list), as.character))

test2 %>% 
  summarise(across(everything(), ~ length(unique(.x)))) %>% View()

test2 %>% 
  summarise(across(everything(), ~ n_distinct(.x))) %>% View()

test2 %>% 
  summarise(across(everything(), ~ length(.x[!is.na(.x)]))) %>% View()

test2 %>% 
  summarise(across(everything(), ~ length(.x[!is.na(.x)])/length(.x))) %>%
  pivot_longer(everything()) %>% 
  View()

test2 %>% 
  summarise(across(where(is.character), ~ n())) %>% View()

test2 %>% 
  summarise(across(where(is.character), ~ length(.x))) %>% View()
