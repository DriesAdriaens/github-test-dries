# Initial installation from dedicated branch in development
# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
# library(remotes)
# install_github("inbo/n2khab@read_soilmap",
#                build_vignettes = TRUE,
#                upgrade = TRUE)
# vignette("v022_example", package = "n2khab")

library(n2khab)
library(knitr)
library(tidyverse)
library(sf)
library(mapview)
library(plotly)
library(units)

help(package = "n2khab")

loc_data <- fileman_up(name = "n2khab_data")
soilmap_simple_path <- file.path(loc_data, "20_processed/soilmap_simple")
dir.create(soilmap_simple_path)
download_zenodo(doi = "10.5281/zenodo.3732903",
                path = soilmap_simple_path)


#?read_soilmap

sm_simple <- read_soilmap()
sm_simple
glimpse(sm_simple)

sm_simple %>%
  st_drop_geometry %>%
  count(bsm_region)

sm_simple %>% 
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  group_by(bsm_region) %>%
  summarise(mean_area = mean(area))

zwin_map <-
  sm_simple %>%
  filter(bsm_region == "Zwin") %>%
  ggplot(aes(fill = bsm_mo_tex)) +
  geom_sf()
zwin_map
zwin_map + coord_sf(datum = st_crs(31370))
zwin_map %>% ggplotly()

sm_simple %>%
  filter(bsm_region == "Zwin") %>%
  mutate(bsm_mo_tex = as.character(bsm_mo_tex)) %>%
  mapview(zcol = "bsm_mo_tex",
          alpha.region = 0.5,
          map.types = "OpenStreetMap")



soilmap_path <- file.path(loc_data, "10_raw/soilmap")
dir.create(soilmap_path)
download_zenodo(doi = "10.5281/zenodo.3387008",
                path = soilmap_path,
                parallel = TRUE)

sm <- read_soilmap(use_processed = FALSE)
sm

sm_mp <-
  sm %>%
  filter(bsm_region == "Middellandpolders")
dim(sm_mp)

#make an interactive map (on top of Wikimedia’s Open Streetmap visualization):
sm_mp %>%
  mutate(bsm_ge_series = as.character(bsm_ge_series)) %>%
  mapview(zcol = "bsm_ge_series",
          alpha.region = 0.2,
          map.types = "Wikimedia",
          alpha = 0)

#calculate surface area (ha) and polygon count per bsm_ge_series:
sm_mp %>%
  mutate(area = st_area(.) %>% set_units("ha")) %>%
  st_drop_geometry %>%
  group_by(bsm_ge_series, bsm_ge_series_explan) %>%
  summarise(area = sum(area) %>% round(2),
            nr_polygons = n()) %>%
  arrange(desc(area)) %>%
  kable

#calculate surface area (ha) and polygon count per soilseries where bsm_ge_series == "D":
sm_mp %>%
  filter(bsm_ge_series == "D") %>%
  mutate(area = st_area(.) %>% set_units("ha")) %>%
  st_drop_geometry %>%
  group_by(bsm_soilseries, bsm_soilseries_explan) %>%
  summarise(area = sum(area) %>% round(2),
            nr_polygons = n()) %>%
  arrange(desc(area)) %>%
  kable

#plot all features where bsm_soilseries == "m.D5":
sm_mp %>%
  filter(bsm_soilseries == "m.D5") %>%
  mapview(zcol = "bsm_ge_series",
          alpha = 0.5,
          map.types = "OpenStreetMap",
          alpha.region = 0.5)


sm_simple_co <- sm_simple %>% 
  filter(bsm_ge_coastalplain == TRUE)
sm_simple_co %>% 
  mapview(map.types = "OpenStreetMap")

sm_co <- sm %>% 
  filter(bsm_ge_coastalplain == TRUE)
sm_co %>%
  mapview(map.types = "OpenStreetMap")
