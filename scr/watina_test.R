library(watina)
library(dplyr)
library(ggplot2)

watina <- connect_watina()

get_locs(watina) %>%
  head(5) %>%
  collect %>%
  as.data.frame

get_locs(watina,
         loc_vec = c("KBRP081", "KBRP090", "KBRP095"))

get_locs(watina,
         loc_vec = c("KBRP081", "KBRP090", "KBRP095"),
         obswells = TRUE,
         collect = TRUE)

get_locs(watina,
         area_codes = c("SIL")) %>% 
  collect %>% 
  as.data.frame

mylocs <- get_locs(watina, area_codes = "KAL")
mylocs %>%
  get_xg3(watina, 2010) %>%
  left_join(mylocs %>%
              select(-loc_wid),
            .) %>%
  collect %>% 
  View()


get_locs(con = watina, loc_vec = c("SILP003"), collect = TRUE, obswells = TRUE) %>% View()

get_xg3(get_locs(con = watina, loc_vec = c("SILP003")), con = watina, startyear = 1900, vert_crs = "local", truncated = FALSE, collect = TRUE) #%>% View()

ggplot(get_xg3(get_locs(con = watina, loc_vec = c("GGVP010")), con = watina, startyear = 1900, vert_crs = "local", truncated = FALSE, collect = TRUE), aes(x = hydroyear, y = hg3_lcl)) +
         geom_line() +
  geom_point()

get_xg3(get_locs(con = watina, loc_vec = c("ASHP013")), con = watina, startyear = 1900, vert_crs = "local", truncated = FALSE, collect = TRUE) %>% 
  eval_xg3_avail(xg3_type = c("L", "H", "V"))

get_xg3(get_locs(con = watina, loc_vec = c("GGVP010")), con = watina, startyear = 1900, vert_crs = "local", truncated = FALSE, collect = TRUE) %>% 
  eval_xg3_series(xg3_type = c("L", "H", "V"), max_gap = 0, min_dur = 2) %>% 
  View()

get_xg3(get_locs(con = watina, loc_vec = c("SILP003")), con = watina, startyear = 1900, vert_crs = "local", truncated = FALSE, collect = TRUE) %>% 
  extract_xg3_series(xg3_type = c("L", "H", "V"), max_gap = 0, min_dur = 4) %>% 
  View()

get_chem(get_locs(con = watina, loc_vec = c("SILP003")), con = watina, "01011900", collect = TRUE) %>% 
  View()
get_chem(get_locs(con = watina, loc_vec = c("SILP003")), con = watina, "01011900", collect = TRUE) %>%
  eval_chem(type = "both") %>% 
  View()
