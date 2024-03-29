#example file get_locs_vmm


# parameter definition:

#con = database connection

#bbox = c(xmin, xmax, ymin, ymax) bbox defined by lambert 72 coordinates.

#stream (character) = river name (in dutch) (eg "Abeek"). Case insensitive

#guess (TRUE/FALSE) = define if stream name is exact or a string of the full river name (eg stream = "Abeek" guess = F returns no data,
#because Abeek is defined as "Abeek - Lossing" in database & geodatabase. Guess = T returns all data with "Abeek" string in this example, including "Abeek - Lossing")

#parameter (character)

#mask = (NULL/shapefile) a shapefile to look within

#buffer = (NULL/numeric) a buffer can be defined both for mask and a HT3260_database but works differently. A buffer combined with a mask will extend the mask with the buffer width and look for points within,
#a buffer combined with a HT3260_database will create a buffer around the VMM sample points.

#collect_HT3260 (TRUE/FALSE) = if TRUE, returns a data frame from vmm database for river segments that are HT3260 habitat. If habitat type of a river segment changes over time,
# data is only collected for the period where river segment is determined as HT3260. If FALSE, a tibble of all vmmdata for given stream, bbox and variable will be returned without
#linkage to HT3260 geodatabase.

#enkele voorbeelden van de functie get_locs_vmm

sbz_gebieden = st_read("c://Qgis/bronnen/SBZ_deelgebieden/SBZ_met_deelgebieden.shp") %>% st_sf(crs = 31370)

#we maken connectie met de inbo-vmm databank oppervlaktewater
vmmchem <- inbodb::connect_inbo_dbase(database_name = "D0113_00_VMMData")

#we vragen de analyseresultaten voor ammoniak op voor habitattype 3260 uit databank v1.7. We nemen een buffer van 10m rond de coordinaten van het staalnamepunt
data_NH4_HT3260 = get_locs_vmm(parameter = "NH4+", collect_HT3260 = T, buffer = 10, geodatabase = HT3260_100mseg_v1_7)

#we vragen de analyseresultaten voor nitraat van alle punten binnen een bepaalde bounding box
data_oPO4_f_bbox = get_locs_vmm(parameter = "oPO4 f", bbox = c(xmin=100731,xmax =103203, ymin =191900, ymax = 193945)) %>% collect
data_oPO4_bbox = get_locs_vmm(parameter = "oPO4", bbox = c(xmin=100731,xmax =103203, ymin =191900, ymax = 193945)) %>% collect

#we vragen alle resultaten op voor nitraat binnen een bepaalde mask, in dit voorbeeld de SBZ-gebieden
data_NO3_SBZ = get_locs_vmm(parameter = 'NO3-', mask = sbz_gebieden) %>% collect

#we vragen alle chemische gegevens op van de voer
data_vmm = get_locs_vmm(stream = "dijle", guess = T) %>% collect



VLAREMII <- readxl::read_xlsx("G:/Mijn Drive/Databanken/VLAREM/VLAREMII_Oppwat_Grndwat.xlsx", sheet = "for_import") %>%
  pivot_longer(cols = !1:8, names_to = "wbody_typecode", values_to = "ref")

# data_vmm %>% 
#   filter(variable %in% c("oPO4", "P t", "date")) %>% 
#   group_by(loc_code, variable) %>% 
#   summarise(n = n(), 
#             yearmin = min(lubridate::year(date)), 
#             yearmax = max(lubridate::year(date))) %>% 
#   View()
# 
# tbl(vmmchem, "MetingFysicoChemiePerMeetpuntJaarTypeParameterBerekening") %>% 
#   filter(MeetpuntCode == "450980", ParameterCode == "P t", Berekening == "Gemiddelde") %>%
#   collect() %>% 
#   View()

data_vmm %>% 
  count(loc_code, wbody_name) %>% 
  View()

library(leaflet)
library(leaflet.minicharts)

pal <- colorFactor(palette = "Accent", domain = data_vmm$wbody_name)

leaflet(data = data_vmm %>% 
          distinct(loc_code, x, y, wbody_name) %>% 
          st_as_sf(coords = c("x", "y"), crs = 31370) %>% 
          st_transform(crs = 4326)) %>%
  addWMSTiles("https://wms.ngi.be/cartoweb/1/service?", layers = "cartoweb_topo",
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              group = "Cartoweb") %>% 
  addTiles(group = "OSM") %>%
  addWMSTiles("https://geoservices.informatievlaanderen.be/raadpleegdiensten/omwrgbmrvl/wms?",
              layers = "Ortho",
              options = WMSTileOptions(format = "image/png", transparant = TRUE),
              group = "Ortho_VL_winter") %>% 
  addWMSTiles("https://inspirepub.waterinfo.be/arcgis/services/Overstromingsgevoelige_gebieden_2017/MapServer/WMSServer?",
              layers = "0",
              options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.5),
              group = "Overstromingsgevoelig") %>% 
  addWMSTiles("https://geoservices.informatievlaanderen.be/raadpleegdiensten/VMM/wms?",
              layers = "RecOvstrGeb",
              options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.5),
              group = "Recent overstroomd") %>% 
  hideGroup(group = c("Overstromingsgevoelig", "Recent overstroomd")) %>% 
  addCircleMarkers(label = ~loc_code, fill = FALSE, weight = 3, opacity = 1, 
                   color = ~pal(wbody_name),
                   # radius = ~ifelse(is.na(gr_mv), 5, 10),
                   labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE,
                                               direction = "right",
                                               opacity = 0.5
                   ),
                   group = "Meetpunten") %>% 
  addMiniMap(toggleDisplay = TRUE,zoomLevelOffset = -5) %>% 
  addLayersControl(
    baseGroups = c("OSM", "Ortho_VL_winter", "Cartoweb"),
    overlayGroups = c("Meetpunten", "Overstromingsgevoelig", "Recent overstroomd"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(pal = pal, values = ~wbody_name,
            # colors = c("red", "blue"), labels = c("grondwater", "oppervlaktewater"),
            group = "Meetpunten", title = "Meetpunttype")


data_vmm %>% 
  mutate(
    HalfjaarType = ifelse(between(lubridate::yday(date), lubridate::yday("2000-05-01"), lubridate::yday("2000-10-30")),
                          "Z", "W")) %>% 
  bind_rows(data_vmm %>% 
              mutate(HalfjaarType = "K")) %>%
  filter(
    !grepl("kanaal", wbody_name, ignore.case = TRUE),
    !grepl("getij", wbody_name, ignore.case = TRUE),
    loc_code %in% c("221550", "221500", "221000", "220500", "220220"),
    wbody_name %in% c("DIJLE I", "DIJLE II"),
    # loc_code %in% c("450980", "451000", "451050", "451445", "450862", "450860", "451100"),
    !variable %in% c("SO4=", "T", "pH", "Secchi", "Cl-"),
    !grepl("^TR", loc_code)
  ) %>% 
  group_by(
    loc_code, wbody_typecode, variable, unit, Jaar = lubridate::year(date), HalfjaarType) %>% 
  summarise(
    Minimum = min(value),
    Maximum = max(value),
    Gemiddelde = mean(value),
    Percentiel90 = quantile(value, probs = 0.9),
    Percentiel10 = quantile(value, probs = 0.1),
    n = n()
  ) %>%
  pivot_longer(cols = -c(loc_code, wbody_typecode, variable, unit, Jaar, HalfjaarType,
                         # grp_diff
                         ),
               names_to = "StatisticType",
               values_to = "Value") %>%
  left_join(VLAREMII, by = c("wbody_typecode" = "wbody_typecode", 
                             "variable" = "Symbool",
                             "StatisticType" = "toetswijze_code",
                             "HalfjaarType" = "JaarType")) %>% 
  # View() %>%
  filter(
    # StatisticType %in% c(
    #   "Gemiddelde"#,
    #   "Percentiel10",
    #   "Percentiel90"
    # ),
    !is.na(toetswijze),
    # Jaar > 1995
    ) %>% 
  # View() %>%
  # berekenen van group variabele om lijngrafiek te onderbreken bij ontbrekende jaren
  group_by(loc_code, variable) %>%
  arrange(loc_code, variable, Jaar) %>%
  mutate(
    diff = ifelse(is.na(Jaar - lag(Jaar)), 0, Jaar - lag(Jaar)),
    cumsum_dif = cumsum(diff > 1),
    grp_diff = as.numeric(loc_code) + percent_rank(cumsum_dif)/10) %>% 
  ggplot(aes(x = Jaar, y = Value, colour = loc_code,
             group = grp_diff
             )) +
  geom_line() +
  geom_point(size = 0.85) +
  geom_hline(aes(yintercept = ref), linetype = "dashed") +
  # facet_grid(paste0(variable, " (", unit, ")") ~ HalfjaarType) +
  facet_wrap(~paste0(variable, " (", unit, ")", ", ", HalfjaarType, "-", toetswijze_code_abbrev),
             scales = "free_y", ncol = 4) +
  # scale_colour_brewer(
  #   # type = "div", palette = 7,
  #   # type = "div", palette = 8,
  #   # type = "qual", palette = "Set1",
  #   # type = "qual", palette = "Set2",
  #   # type = "qual", palette = "Set3",
  #   type = "qual", palette = "Dark2",
  #   # type = "div", palette = 9
  # ) +
  labs(x = "Jaar", y = "Waarde", colour = "Meetpunt") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # panel.grid.minor.x = element_line(linetype = "dashed"),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
  )

ggsave("test.png", dpi = 300, width = 21, units = "cm")


