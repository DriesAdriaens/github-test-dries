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
surveys <- get_inboveg_survey(con, survey_name = "%N2000meetnet%", collect = TRUE)

# 1.2 full table
colnames(tbl(con, "ivSurvey"))
surveys_ <- tbl(con, "ivSurvey") %>% 
  filter(Name %like% "%level%") %>%
  # filter(str_like(Name, pattern = "%level%")) %>% 
  collect()


# 2. Header #####
# 2.1 inbodb #####
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
         # year(VagueDateBegin) > 1980
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
  ggplot() + geom_bar(aes(x = str_wrap(RecordingScale, 70), y = n), stat = "identity") + coord_flip() + labs(x = "Opnameschaal", y = "Aantal opnamen")

opname_selectie %>% 
  filter(
    str_like(Name.x, 
             pattern = "%n2000meetnet%",
             )
  ) %>% 
  group_by(Name.x, Description, SurveyId, RecordingScale, year(VagueDateBegin)) %>% 
  count() %>% 
  View()



headers <- # Header gegevens van opnamen die in aanmerking komen voor TWINSPAN van 2190
  get_inboveg_header(con, survey_name = c(
  "N2000_Duinen_fieldapp",
  "N2000meetnet_Duinen_BHM",
  "Kust_PQ"),
  multiple = TRUE) %>%
  filter(RecTypeID == 2) %>% 
  left_join(
    get_inboveg_classification(con) %>% select(-Name),
    by = "RecordingGivid") %>% 
  left_join(
    get_inboveg_relation_recording(con) %>% select(Child_GIVID, Parent_GIVID),
    by = c("RecordingGivid" = "Child_GIVID")) %>% 
  collect() %>% 
  filter((grepl("N2000", Name) & grepl("2190|2130|2170", Classif))|
           (is.na(Classif) & grepl("bd_15|be_03|bh_07|dh_01|dh_02|dh_04|dh_07|dh_13|dh_16|dp_03|fo_01|fo_02|gd_01|gd_02|gd_03|gd_04|gh_07|ha_01|ha_03|ha_04|ha_08|ha_11|ha_13|ha_15|ha_16|ha_33|ha_35|ha_37|ha_39|ha_40|hs_01|hs_20|ij_15|ka_02|nd_12|nd_20|nd_22|oh_06|ov_02|ov_93|ov_94|ov_95|ov_96|ov_98|pa_01|sb_03|sd_03|sl_03|ty_02|ty_05|ty_09|ty_11|ty_14|wd_03|wn_01|wn_05|wn_06|wn_07|wn_08|wn_09|wn_10|wn_11|wn_14|wn_16|wn_17|wn_18|wn_19|wn_20|wn_21|wn_27|wn_28|wn_31|wn_44|wn_45|wn_46|wn_47|wn_51|wn_52|wn_53|wn_54|wn_55|wn_57|wn_58|wn_60|wn_61|wn_62|wn_63|wn_64|wn_65|wn_67|wz_01|wz_03|wz_06|wz_07|wz_11|wz_13|wz_14|wz_15|wz_16|wz_28|wz_30|wz_31|wz_49|wz_51|wz_52|wz_53|wz_54|wz_61|wz_62|wz_63|wz_65|wz_66|wz_67|wz_68|wz_69|wz_74|zw_02|zw_06|zw_07|zw_08|zw_09|zw_18|zw_19|zw_29|zw_31|zw_92|zw_93|zw_97|gh_05", UserReference)))

headers %>% # Aantal herhalingen van de pq's uit de Kust_PQ survey (PINK), op basis van de UserReference
  filter(grepl("Kust_PQ", Name)) %>% 
  mutate(UserRef_abbrev = str_sub(UserReference, 1, 5)) %>% 
  group_by(UserRef_abbrev) %>% 
  count() %>% View()

headers %>% # Aantal herhalingen van de pq's op basis van de Parent-Child relationship (Container/Emmer)
  group_by(Name, Parent_GIVID) %>% 
  arrange(ymd(VagueDateBegin)) %>% 
  summarise(
    n = n(),
    years = paste(year(ymd(VagueDateBegin)), collapse = ","),
    user_ref = paste(UserReference, collapse = ",")
  ) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  View()





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
