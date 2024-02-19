library(glue)
library(DBI)
library(assertthat)
library(dplyr)
library(inbodb)
library(ggplot2)
library(inboggvegan)
library(vegan)
library(tidyr)
library(stringr)

# Make connection ----

con <- connect_inbo_dbase("D0010_00_Cydonia")

# Test INBOVEG functions of inbodb package ----

survey <- get_inboveg_survey(con, "SBO%", collect = TRUE)
record <- get_inboveg_recordings(con, "SBO%", collect = TRUE)

record %>% count(CoverageCode) %>% ggplot() + geom_bar(aes(CoverageCode, n), stat = "identity")
record %>% count(LayerCode)

header <- get_inboveg_header(con, rec_type = "classic", survey_name = "%SBO%", collect = TRUE)
header %>% count(Observer) %>% ggplot() + 
  geom_bar(aes(Observer, n), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

header %>% filter(grepl("Dries", Observer) & grepl("Siege", Observer)) %>% count(Observer)

clsf <- get_inboveg_classification(con, classif = "3130", collect = TRUE)
clsf %>% count(Name)                

clsf <- get_inboveg_classification(con)
clsf %>% count(ActionGroup)
temp <- clsf %>%
  filter(ActionGroup == "N2k") %>% 
  collect() %>% 
  count(Name) %>%
  arrange(-n) %>%
  View()
temp <- test %>% filter(ActionGroup == "N2k") %>% count(Classif) %>% collect() %>% arrange(-n)

library(tidyverse)
library(inbodb)

con <- connect_inbo_dbase("D0010_00_Cydonia")
con_futon <- connect_inbo_dbase("D0013_00_Futon")

# 1. Survey ####
# 1.1 inbodb ####
surveys <- get_inboveg_survey(con, survey_name = "%level%", collect = TRUE)

# 1.2 full table
colnames(tbl(con, "ivSurvey"))
surveys_ <- tbl(con, "ivSurvey") %>% 
  filter(Name %like% "%level%") %>%
  # filter(str_like(Name, pattern = "%level%")) %>% 
  collect()


# 2. Header ####
# 2.1 inbodb ####
headers <- get_inboveg_header(con, survey_name = "%level%") %>% 
  mutate(year = year(VagueDateBegin)) %>%
  group_by(Name, SurveyId, LocationCode, Latitude, Longitude, year) %>%
  summarise(n = count(RecordingGivid)) %>%
  arrange(LocationCode, year, Name) %>%
  collect()

# 3. Recording ####
# 3.1 inbodb ####
recordings <- get_inboveg_recording(con, survey_name = "%level%")

recordings_header <- recordings %>%
  left_join(get_inboveg_header(con, survey_name = "%level%"), by = "RecordingGivid") %>% 
  # filter(str_like(LocationCode, pattern ="%Aalmoezenijbos%"), year(VagueDateBegin) == 1988) %>% 
  collect()

# 4. Qualifiers ####
# 4.1 inbodb ####
qualifiers <- get_inboveg_qualifier(con, survey_name = "%level%")

# 5. Classification ####
# 5.1 inbodb ####
classifications <- get_inboveg_classification(con)



opname_selectie <- get_inboveg_header(con, rec_type = "Classic") %>% 
  left_join(get_inboveg_recording(con) %>% 
              group_by(RecordingGivid, RecordingScale) %>% 
              summarise(n_spec = n()) %>% 
              ungroup() %>% 
              distinct(RecordingGivid, RecordingScale, n_spec), 
            by = "RecordingGivid") %>% 
  filter(Latitude != 500, !str_like(RecordingScale, pattern = "%tansley%")) %>% 
  # left_join(get_inboveg_classification(con), by = "RecordingGivid") %>% 
  collect() %>%
  # left_join(get_inboveg_qualifier(con), by = "RecordingGivid") %>% 
  left_join(tbl(con_futon, "ftActionGroupList") %>%
              filter(ActionGroup == "cover") %>%
              collect(),
            by = c("RecordingScale" = "Description")) %>% 
  filter(!str_like(ListName, pattern = "%tansley%|%twinspan%"),
         year(VagueDateBegin) > 1980
  ) %>% 
  left_join(tbl(con, "ivSurvey") %>% collect(), by = c("SurveyId" = "Id"))

# opname_selectie %>% write_csv("opname_selectie_INBOVEG_20240219.csv")

opname_selectie %>%
  mutate(Jaar = year(VagueDateBegin)) %>% 
  group_by(Jaar) %>% 
  summarise(n = n()) %>%
  arrange(Jaar) %>% 
  ggplot(aes(x = Jaar)) +
  geom_bar(aes(y = n), stat = "identity") +
  labs(y = "Aantal opnamen")

opname_selectie %>% 
  count(RecordingScale) %>% 
  ggplot() + geom_bar(aes(x = RecordingScale, y = n), stat = "identity") + coord_flip()



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

## Transform data to wide format and replace RecordingGivid with UserReference as site name
testdata_trans <- testdata %>% 
  select(RecordingGivid, Sci_abbr, PctValue) %>% 
  spread(key = Sci_abbr, value = PctValue, fill = 0) %>% 
  left_join(testdata_header %>% 
             select(RecordingGivid, UserReference),
           by = "RecordingGivid") %>% 
  select(UserReference, everything(), -RecordingGivid) %>% 
  data.frame(row.names = "UserReference")
rownames(testdata_trans) <- testdata_trans$UserReference

## Quick vegan analysis CA
CA <- cca(testdata_trans)
ggscreeplot(CA, type = "both")
ggbiplot_vegan(CA)

CA_plot <- ordiplot(CA, type = "text", xlim = c(-2, 4), ylim = c(-2, 0.5), display = "species")
identify(CA_plot, "species")
identify(CA_plot, "sites")

ggplot() +
  geom_point(data = data.frame(CA$CA$u), aes(x = CA1, y = CA2)) +
  geom_point(data = data.frame(CA$CA$v), aes(x = CA1, y = CA2), colour = "red", alpha = 0.5)

ggbiplot_vegan(CA, choices = c(1,3), species_geom = "point")
