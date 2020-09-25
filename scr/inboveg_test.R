library(glue)
library(DBI)
library(assertthat)
library(dplyr)
library(inbodb)
library(ggplot2)
library(vegan)
library(tidyverse)
library(stringr)

# Make connection ----

con <- connect_inbo_dbase("D0010_00_Cydonia")

# Test INBOVEG functions of inbodb package ----

test <- get_inboveg_survey(con, "SBO%", collect = TRUE)
test <- get_inboveg_recordings(con, "SBO%", collect = TRUE)

test %>% count(CoverageCode) %>% ggplot() + geom_bar(aes(CoverageCode, n), stat = "identity")
test %>% count(LayerCode)

test <- get_inboveg_header(con, rec_type = "classic", survey_name = "%SBO%", collect = TRUE)
test %>% count(Observer) %>% ggplot() + 
  geom_bar(aes(Observer, n), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

test %>% filter(grepl("Dries", Observer) & grepl("Siege", Observer)) %>% count(Observer)

test <- get_inboveg_classification(con, classif = "3130", collect = TRUE)
test %>% count(Name)                

test <- get_inboveg_classification(con)
test %>% count(ActionGroup)
temp <- test %>%
  filter(ActionGroup == "N2k") %>% 
  count(Name) %>%
  arrange(-n) %>%
  collect() %>% 
  View()
temp <- test %>% filter(ActionGroup == "N2k") %>% count(Classif) %>% collect() %>% arrange(-n)

# Load testdata from INBOVEG for CA in vegan package ----

testdata <- get_inboveg_recordings(con, "SBO%overstr%", collect = TRUE)
testdata_header <- get_inboveg_header(con, "SBO%overstr%", collect = TRUE)

## Abbreviate plant species names
Sci_uni <- testdata %>% 
  select(ScientificName) %>% 
  distinct() %>%
  mutate(temp = str_split_fixed(ScientificName, pattern = " ", n = 3),
         Sci_bin = str_c((temp[,1]), temp[,2], sep = " "),
         Sci_abbr = str_to_lower(make.cepnames(Sci_bin))) %>% 
  select(-temp) %>% 
  arrange(ScientificName)
testdata <- testdata %>% left_join(Sci_uni, by = "ScientificName")

## Transform data to wide format
testdata_trans <- testdata %>% 
  select(RecordingGivid, Sci_abbr, PctValue) %>% 
  spread(key = Sci_abbr, value = PctValue, fill = 0) %>% 
  left_join(testdata_header %>% 
             select(RecordingGivid, UserReference),
           by = "RecordingGivid") %>% 
  select(UserReference, everything(), -RecordingGivid)
