library(n2khab)
library(knitr)
library(tidyverse)
library(sf)
library(mapview)
library(plotly)
library(units)

#help(package = "n2khab")

# #data downloaden naar n2khab_data folder
# 
# loc_data <- fileman_up(name = "n2khab_data")
# path_ <- file.path(loc_data, "20_processed/watersurfaces_hab")
# dir.create(path_)
# download_zenodo(doi = "10.5281/zenodo.3374645",
#                 path = path_)


#inlezen watersurfaces_hab data source
wsf_hab <- read_watersurfaces_hab(interpreted = TRUE)
wsf_hab$watersurfaces_polygons %>% glimpse()
wsf_hab$watersurfaces_types %>% glimpse()

#selecteren
wsf_hab$watersurfaces_polygons %>% 
  filter(grepl("\\+", polygon_id_habitatmap)) %>% 
  mapview(map.types = "OpenStreetMap")

wsf_hab$watersurfaces_polygons %>% 
  filter(grepl("\\+", polygon_id_habitatmap)) %>% 
  st_drop_geometry() %>% 
  count()

wsf_hab$watersurfaces_polygons %>% 
  filter(grepl("\\+", polygon_id_habitatmap)) %>% 
  left_join(wsf_hab$watersurfaces_types) %>%
  count()

wsf_hab$watersurfaces_polygons %>% 
  filter(grepl("\\+", polygon_id_habitatmap)) %>% 
  left_join(wsf_hab$watersurfaces_types) %>%
  #st_drop_geometry() %>% 
  group_by(polygon_id) %>% 
  mutate(n_type = n(),
         types = paste(type, collapse = ";")) %>% 
  ungroup() %>% 
  mapview(zcol = "n_type",
          map.types = "OpenStreetMap",
          alpha = 0.5)

wsf_hab$watersurfaces_polygons %>% 
  filter(grepl("\\+", polygon_id_habitatmap)) %>% 
  left_join(wsf_hab$watersurfaces_types) %>%
  #st_drop_geometry() %>% 
  group_by(polygon_id) %>% 
  summarise(n_type = n(),
         types = paste(type, collapse = ";")) %>% 
  ungroup() %>% 
  mapview(zcol = "n_type",
          map.types = "OpenStreetMap",
          alpha = 0.5)

wsf_hab$watersurfaces_polygons %>% 
  filter(is.na(polygon_id_ws)) %>% 
  left_join(wsf_hab$watersurfaces_types) %>%
  group_by(type) %>% 
  summarise(type_cnt = n()) %>% 
  ggplot(aes(type, type_cnt)) +
  geom_col() +
  geom_label(aes(label = type_cnt)) +
  labs(title = "Frequency of (sub)types within polygons not represented in watersurfaces layer")

wsf_hab$watersurfaces_polygons %>% 
  filter(!is.na(polygon_id_ws)) %>% 
  left_join(wsf_hab$watersurfaces_types) %>%
  group_by(type) %>% 
  summarise(type_cnt = n()) %>% 
  ggplot(aes(type, type_cnt)) +
  geom_col() +
  geom_label(aes(label = type_cnt)) +
  labs(title = "Frequency of (sub)types within polygons represented in watersurfaces layer")