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
library(sf)
library(leaflet)

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

# Opvragen data inboveg update referentiewaarden GXG, partim duinen (2190) ----

headers <- # Header gegevens van opnamen die in aanmerking komen voor TWINSPAN van 2190; gebaseerd op combinatie van survey en classificatie (N2000 habitattype), en binnen de survey "Kust_PQ" specifiek op de pq's die volgens Sam in aanmerking komen voor de TWINSPAN (zie https://docs.google.com/spreadsheets/d/1im195IWtM1rzG9IemR_zfGl_b16RtykC/edit?usp=sharing&ouid=112596671114122444874&rtpof=true&sd=true, tabblad PQ)
  get_inboveg_header(con, survey_name = c(
  "N2000_Duinen_fieldapp",
  "N2000meetnet_Duinen_BHM",
  "Kust_PQ"),
  multiple = TRUE) %>%
  # filter(RecTypeID == 2) %>% 
  left_join(
    get_inboveg_classification(con) %>% select(-Name),
    by = "RecordingGivid") %>% 
  left_join(
    get_inboveg_relation_recording(con) %>% select(Child_GIVID, Parent_GIVID),
    by = c("RecordingGivid" = "Child_GIVID")) %>% 
  collect() %>% 
  filter((grepl("N2000", Name) & grepl("2190|2130|2170", Classif))|
           (is.na(Classif) & grepl("bd_15|be_03|bh_07|dh_01|dh_02|dh_04|dh_07|dh_13|dh_16|dp_03|fo_01|fo_02|gd_01|gd_02|gd_03|gd_04|gh_07|ha_01|ha_03|ha_04|ha_08|ha_11|ha_13|ha_15|ha_16|ha_33|ha_35|ha_37|ha_39|ha_40|hs_01|hs_20|ij_15|ka_02|nd_12|nd_20|nd_22|oh_06|ov_02|ov_93|ov_94|ov_95|ov_96|ov_98|pa_01|sb_03|sd_03|sl_03|ty_02|ty_05|ty_09|ty_11|ty_14|wd_03|wn_01|wn_05|wn_06|wn_07|wn_08|wn_09|wn_10|wn_11|wn_14|wn_16|wn_17|wn_18|wn_19|wn_20|wn_21|wn_27|wn_28|wn_31|wn_44|wn_45|wn_46|wn_47|wn_51|wn_52|wn_53|wn_54|wn_55|wn_57|wn_58|wn_60|wn_61|wn_62|wn_63|wn_64|wn_65|wn_67|wz_01|wz_03|wz_06|wz_07|wz_11|wz_13|wz_14|wz_15|wz_16|wz_28|wz_30|wz_31|wz_49|wz_51|wz_52|wz_53|wz_54|wz_61|wz_62|wz_63|wz_65|wz_66|wz_67|wz_68|wz_69|wz_74|zw_02|zw_06|zw_07|zw_08|zw_09|zw_18|zw_19|zw_29|zw_31|zw_92|zw_93|zw_97|gh_05", UserReference))) %>% 
  rename(Latitude_orig = Latitude, Longitude_orig = Longitude)

plot(st_as_sf(headers, coords = c("Longitude_orig", "Latitude_orig"), crs = 4326, na.fail = FALSE)) # Hieruit blijkt dat voor een aantal punten de coördinaten niet kloppen; vermoeden is dat X en Y (GivenLongitude, GivenLatitude met epsg 31370) omgedraaid ingegeven zijn; GivenLongitude en GivenLatitude worden echter niet opgehaald door get_inboveg_* functies, dus ...


# alternatief via bevraging originele INBOVEG tabellen (en niet via inbodb get_inboveg* functies) omdat blijkt dat voor sommige opnamen de coördinaten niet kloppen, wellicht omdat de Lambert X en Y omgewisseld werden (en als zodanig vertaald naar lat/lon); maar via inbodb komen die originele coördinaten niet mee (GivenLon*, GivenLat*; bij epsg 31370), terwijl daarop net de correctie moet gebeuren, waarna de omzetting naar WGS84 gebeurt (Longitude, Latitude; epsg 4326)

headers <- 
  tbl(con, "ivRecording") %>% 
  left_join(tbl(con, "ivSurvey"), by = c("SurveyId" = "Id"), 
            keep = FALSE, suffix = c("_record", "_survey")) %>% 
  left_join(tbl(con, "ivRecordingRelation") %>% 
              inner_join(tbl(con, "ivRecording") %>% 
                           select(Id, RecordingGivid) %>% 
                           rename("Parent_GIVID" = "RecordingGivid"),
                         by = c("ParentId" = "Id"), keep = FALSE),
            by = c("Id" = "RecordingId"), keep = FALSE) %>%
  left_join(tbl(con, "ivRLClassification") %>%
              left_join(tbl(con, "ivRLResources"),
                        by = c("ClassifResource" = "ResourceGIVID"),
                        keep = FALSE, suffix = c("", "_classif")) %>% 
              left_join(tbl(con, "ivRLResources"),
                        by = c("CoverResource" = "ResourceGIVID"), 
                        keep = FALSE, suffix = c("", "_cover")),
            by = c("Id" = "RecordingID"), keep = FALSE) %>% 
  select(-starts_with("id")) %>% # anders geeft SQL problemen ...
  filter(Name %like% "%duin%" | Name == "Kust_PQ") %>% 
  # filter(RecTypeID == 2) %>% # best initieel alle opnametypes meenemen en later filteren; voor de coördinaten van de "ouder" opnamen (Parent) is RecTypeID = 4:5 (resp. Classic-emmer en Classic-ketting), voor de "child" gaat het om RecTypeID = 2 (Classic)) 
  collect() %>% 
  filter((grepl("N2000", Name) & grepl("2190|2130|2170", Classif))|
           (is.na(Classif) & grepl("bd_15|be_03|bh_07|dh_01|dh_02|dh_04|dh_07|dh_13|dh_16|dp_03|fo_01|fo_02|gd_01|gd_02|gd_03|gd_04|gh_07|ha_01|ha_03|ha_04|ha_08|ha_11|ha_13|ha_15|ha_16|ha_33|ha_35|ha_37|ha_39|ha_40|hs_01|hs_20|ij_15|ka_02|nd_12|nd_20|nd_22|oh_06|ov_02|ov_93|ov_94|ov_95|ov_96|ov_98|pa_01|sb_03|sd_03|sl_03|ty_02|ty_05|ty_09|ty_11|ty_14|wd_03|wn_01|wn_05|wn_06|wn_07|wn_08|wn_09|wn_10|wn_11|wn_14|wn_16|wn_17|wn_18|wn_19|wn_20|wn_21|wn_27|wn_28|wn_31|wn_44|wn_45|wn_46|wn_47|wn_51|wn_52|wn_53|wn_54|wn_55|wn_57|wn_58|wn_60|wn_61|wn_62|wn_63|wn_64|wn_65|wn_67|wz_01|wz_03|wz_06|wz_07|wz_11|wz_13|wz_14|wz_15|wz_16|wz_28|wz_30|wz_31|wz_49|wz_51|wz_52|wz_53|wz_54|wz_61|wz_62|wz_63|wz_65|wz_66|wz_67|wz_68|wz_69|wz_74|zw_02|zw_06|zw_07|zw_08|zw_09|zw_18|zw_19|zw_29|zw_31|zw_92|zw_93|zw_97|gh_05", UserReference))) %>% 
  mutate(across(starts_with("Given"), ~as.numeric(.)),
         across(contains(c("VagueDateBegin", "VagueDateEnd")), ~ymd(.)),
         # Cover = as.numeric(Cover),
         X = ifelse(GivenLongitude < 100000, GivenLongitude, GivenLatitude), # corrigeren van aantal opnames waarvoor x en y werden omgewisseld bij de invoer; als reeds in epsg 4326 [-180:180], dan behouden we Longitude; of als Lambert X oostelijk van 100000 i.e. buiten de kuststreek), dan dienen de coördinaten omgewisseld
         Y = ifelse(GivenLatitude < 180000 & grepl("31370", CoordinateRefSystem), GivenLongitude, GivenLatitude), # idem voorgaande, maar dan voor Latitude/Lambert Y; verkeerde opnamen liggen nu in de Ardennen ergens, i.p.v. aan de oostkust, vandaar 180000; voor Lambert Y < 180000 wordt Latitude (Y) omgewisseld met Longitude (X)
         coords_changed = ifelse(GivenLongitude == X, 0, 1)) %>% 
  left_join(tbl(con_futon, "ftActionGroupList") %>% 
              select(ListGIVID, ActionGroup, ListName),
            by = c("ActionGroup_cover" = "ActionGroup", "ListName_cover" = "ListName"),
            keep = FALSE, copy = TRUE) %>%
  left_join(tbl(con_futon, "ftCoverValues") %>% 
              select(ListGIVID, Code, PctValue),
            by = c("ListGIVID", "Cover" = "Code"), keep = FALSE, copy = TRUE) %>% 
  rename(Latitude_orig = Latitude, Longitude_orig = Longitude) %>% 
  select(-ListGIVID)

headers <- # uniforme berekening van Lon en Lat (epsg 4326) obv gecorrigeerde coördinaten; dus omrekening van epsg 31370 naar 4326, of behoud van Lon/Lat als reeds in epsg 4326; resultaat is berekening van 2 nieuwe velden (Lon en Lat) met coördinaten in epsg 4326
  rbind(
    headers %>% 
      filter(grepl("31370", CoordinateRefSystem)) %>%
      cbind(headers %>% #transformatie van epsg 31370 (Lambert) naar 4326 (WGS84 ofte Lon/Lat)
              filter(grepl("31370", CoordinateRefSystem)) %>% 
              st_as_sf(coords = c("X", "Y"), crs = 31370, na.fail = FALSE) %>%
              st_transform(crs = 4326) %>%
              st_coordinates() %>%
              as.data.frame() %>% 
              rename("Longitude" = "X", "Latitude" = "Y")),
    headers %>% # behoud van Lon/Lat indien epsg 4326; voor opnamen met Lon/Lat lijken de coördinaten immers te kloppen
      filter(grepl("4326", CoordinateRefSystem)) %>% 
      mutate(
        Longitude = X,
        Latitude = Y))

plot(st_as_sf(headers, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = FALSE))

dubbele <- headers %>% # Check voor dubbele records n.a.v. Classification (complexen van meerdere habitattypen, met name grijze duinen 2130 en kruipwilgstruweel 2170)
  group_by(RecordingGivid) %>% 
  count() %>% 
  filter(n > 1) %>% 
  left_join(headers)

headers <- headers %>% # dubbele records n.a.v. Classification verwijderen
  group_by(RecordingGivid) %>% 
  slice_max(order_by = PctValue, n = 1, with_ties = FALSE) %>% 
  ungroup()
  
headers <- headers %>% # UserReference_ extraheren om Childs (RecTypeID = 2) te groeperen en te kunnen linken met Parent (voor coördinaten en relevante kopgegevens) voor records met ontbrekende Parent_GIVID (survey N2000_Duinen_fieldapp)
  mutate(test = str_count(UserReference, "_"),
         test2 = str_locate(UserReference, "_"),
         UserReference_ = ifelse(is.na(test2[,2]), UserReference, ifelse(test2[,2] <= 3,
                                                                         str_sub(UserReference, 1, 5),
                                                                         str_sub(UserReference, 1, test2[,2] - 1))),
         test = NULL,
         test2 = NULL)

headers <- headers %>% # Lijst van variabelen wat inperken
  select(RecordingGivid, LocationCode, Latitude, Longitude, Latitude_orig, Longitude_orig,
         UserReference, UserReference_, RecTypeID, VagueDateBegin, SurveyGivid, Name, ParentId, Parent_GIVID, Classif, ActionGroup, ListName, PctValue)

headers <- headers %>% # alternatieve Parent_GIVID.y toevoegen, met aanvullingen voor survey N2000_Duinen_fieldapp o.b.v. survey N2000meetnet_Duinen_BHM
  left_join(headers %>% 
              filter(!is.na(Parent_GIVID), RecTypeID == 2) %>% 
              select(UserReference_, Classif, Parent_GIVID) %>% 
              distinct(),
            by = c("UserReference_", "Classif"))

headers <- headers %>% # ontbrekende coördinaten (Lat/Lon > 500) van de "Classic" opnamen (RecTypId = 2) aanvullen met die van de Parents (Classic-emmer of Classic-ketting; RecTypeId = 4:5) obv Parent_GIVID.y
  left_join(
    # get_inboveg_header(con, survey_name = c("N2000_Duinen_fieldapp", "N2000meetnet_Duinen_BHM", "Kust_PQ"), 
    #                    multiple = TRUE) %>% 
    headers %>% 
      filter(RecTypeID > 3) %>% 
      # collect() %>% 
      semi_join(headers, by = c("RecordingGivid" = "Parent_GIVID.y")) %>% 
      select(RecordingGivid, Longitude, Latitude),
    by = c("Parent_GIVID.y" = "RecordingGivid")) %>% 
  mutate(
    across(starts_with(c("Longitude", "Latitude")), ~ifelse(.x > 100, NA, .x)),
    # Longitude = coalesce(pick(starts_with("Longitude"))),
    # Latitude = coalesce(pick(starts_with("Latitude"))),
    Longitude = coalesce(Longitude.x, Longitude.y),
    Latitude = coalesce(Latitude.x, Latitude.y)
  ) %>% 
  select(-starts_with(c("Longitude.", "Latitude."))) %>%
  filter(RecTypeID == 2)

# Op kaart

library(watina)



watina <- read_delim("C:/GDRIVE/GIS/Grondwater/WATINA/TBL_Meetpunt_details_20240130.txt", delim = ";", locale = locale(encoding = "ISO-8859-1"))
watina_pb_sf <- st_as_sf(watina %>% 
                        filter(str_like(MeetpuntType, pattern = "%peilbuis%")),
                        coords = c("X", "Y"), crs = 31370, na.fail = FALSE)

pal <- colorFactor(topo.colors(3), domain = headers$Name)

wms_cartoweb_be <- "https://cartoweb.wms.ngi.be/service"
wms_ortho_wmr <- "https://geo.api.vlaanderen.be/OMWRGBMRVL/wms"
wms_dhm <- "https://geo.api.vlaanderen.be/DHMV/wms"

labels_pb <- sprintf("<strong>%s</strong><br/>GHG_n = %d<br/>metingen van %d tot %d<br/>GHG:%.2f, GVG:%.2f, GLG:%.2f<br/><a>https://watina.inbo.be/peilpunt/%s</a>", watina_pb_sf$MeetpuntCode, watina_pb_sf$GHG_n, watina_pb_sf$Year_first, watina_pb_sf$Year_last, watina_pb_sf$GHG_mv, watina_pb_sf$GVG_mv, watina_pb_sf$GLG_mv, watina_pb_sf$PeilpuntCode) %>% lapply(htmltools::HTML)

leaflet() %>%
  setView(2.585897, 51.093438, 14) %>% 
  addTiles(group = "OSM") %>% 
  addWMSTiles(wms_cartoweb_be, layers = "topo_grey", group = "Cartoweb", options = WMSTileOptions(format = "image/png")) %>% 
  addWMSTiles(wms_ortho_wmr, layers = "Ortho", group = "Orthofoto's", options = WMSTileOptions(format = "image/png")) %>% 
  addWMSTiles(wms_dhm, layers = "DHMV_II_HILL_25cm", group = "Hillshade DTMVLII25cm", options = WMSTileOptions(format = "image/png")) %>% 
  addCircles(data = watina_pb_sf %>% st_transform(crs = 4326), group = "peilbuizen", color = "red", weight = 2, opacity = 0.6, fill = 0, radius = ~ifelse(GHG_n >= 5, 20, 10), label = labels_pb, popup = labels_pb) %>%
  # addMarkers(data = watina_pb_sf %>% st_transform(crs = 4326), group = "peilbuizen", label = labels_pb, popup = labels_pb, clusterOptions = markerClusterOptions()) %>% 
  addCircles(data = headers, lng = ~Longitude, lat = ~Latitude, color = ~pal(Name), weight = 3, opacity = 0.6, fill = 0, label = ~UserReference_, group = "INBOVEG opnamen") %>% 
  addLegend(pal = pal, values = headers$Name, position = "bottomleft", opacity = 1) %>% 
  addLayersControl(overlayGroups = c("INBOVEG opnamen", "peilbuizen"), 
                   baseGroups = c("Cartoweb", "OSM", "Orthofoto's", "Hillshade DTMVLII25cm"), options = layersControlOptions(collapsed = FALSE)) %>%   # hideGroup("OSM")
  addMiniMap(toggleDisplay = TRUE,zoomLevelOffset = -4)

# Enkele checks

headers %>% # er ontbreken dus nog parent_givid's voor records uit survey "N2000_Duinen_fieldapp"
  filter(is.na(Parent_GIVID.y)) %>% 
  View()

headers %>% # verdeling van opnames (Classic) over surveys 
  count(Name)
  
headers %>% # verdeling van pq's over surveys
  distinct(Name, UserReference_) %>% 
  count(Name)

headers %>% 
  distinct(Name, UserReference_) %>% 
  count()

headers %>% # Aantal herhalingen van de pq's uit de Kust_PQ survey (PINK), op basis van de UserReference
  filter(grepl("Kust_PQ", Name)) %>% 
  mutate(UserRef_abbrev = str_sub(UserReference, 1, 5)) %>% 
  group_by(UserRef_abbrev) %>% 
  count() %>% 
  arrange(-n) %>% 
  View()

headers %>% # Aantal herhalingen van de pq's op basis van de Parent-Child relationship (Container/Emmer)
  group_by(Name, Parent_GIVID.y, UserReference_) %>% 
  summarise(
    n = n(),
    years = paste(year(ymd(VagueDateBegin)), collapse = ","),
    user_refs_coll = paste(UserReference, collapse = ",")
  ) %>%  
  ungroup() %>% 
  arrange(-n) %>% 
  View()

# Opnamen zelf dan nog toevoegen

recordings <- headers %>% 
  left_join(get_inboveg_recording(con, recording_givid = headers$RecordingGivid, multiple = TRUE, collect = TRUE) %>% 
              filter(LayerCode == "K") %>% 
              select(-Name, -UserReference),
            by = "RecordingGivid", suffix = c("", "_record"))




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
