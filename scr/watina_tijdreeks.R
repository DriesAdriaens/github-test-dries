library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)


watina <- connect_watina()

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     # area_codes = "GGV",
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP033"),
                     loc_validity = c("VLD", "ENT"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     # filterdepth_range = c(0, 3),
                     # filterdepth_guess = FALSE,
                     # filterdepth_na = FALSE,
                     # obswells = FALSE,
                     # obswell_aggr = c("latest", "latest_fd", "latest_sso", "mean"),
                     collect = FALSE)
tijdreeks <-
  locaties %>% 
  rename(mvTAW = soilsurf_ost) %>% 
  inner_join(tbl(watina, "FactPeilMeting"), by = c("loc_wid" = "MeetpuntWID")) %>% 
  inner_join(tbl(watina, "DimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  dplyr::collect()

# 2. figuur van tijdreeks(en) voor gewenste meetpunten ----


plot_tijdreeks <- ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum),
                y = mMaaiveld,
                # y = mTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1),
  #                 xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2018-01-01"), ymd("2020-08-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = "Meetpunt") +
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  geom_point(data = tijdreeks %>%
               filter(ReprPeriode > 30),
             aes(x = date(Datum), y = mMaaiveld))
  ## accentueren van specifieke tijdreeks
  # geom_line(data = tijdreeks %>%
  #               filter(loc_code == "ASHP013"),
  #             aes(x = date(Datum), y = mMaaiveld),
  #             colour = "black", size = 1.5)
  
temp <- get_xg3(locs = locaties, con = watina,
                startyear = 1900, endyear = 2020,
                vert_crs = "local") %>%
  eval_xg3_series(xg3_type = c("L", "H", "V"), max_gap = 0, min_dur = 2)

ghg <- temp$ser_mean[temp$loc_code == "GGVP013" & temp$xg3_variable == "hg3_lcl"]
glg <- temp$ser_mean[temp$loc_code == "GGVP013" & temp$xg3_variable == "lg3_lcl"]

plot_tijdreeks + geom_hline(yintercept = ghg, size = 1, linetype = "dashed") +
  geom_hline(yintercept = glg, size = 1, linetype = "dashed")

reftab <- read.csv("C:/Users/dries_adriaens/Documents/niche_vegetation.csv")

ggplot(reftab %>% 
         select(-nutrient_level, - acidity, -management, -inundation) %>% 
         distinct()) +
  geom_linerange(aes(x = factor(veg_code), ymin = -mhw_min, ymax = -mhw_max,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  labs(x = "Vegetatietype", y = "bereik ghg (cm-mv)", colour = "bodemtype") +
  ylim(c(-300, 50))

ggplot(reftab %>% 
         select(-nutrient_level, - acidity, -management, -inundation) %>% 
         distinct()) +
  geom_linerange(aes(x = factor(veg_code), ymin = -mlw_min, ymax = -mlw_max,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  labs(x = "Vegetatietype", y = "bereik glg (cm-mv)", colour = "bodemtype") +
  ylim(c(-300, 50))

test <- reftab %>% 
  select(-nutrient_level, - acidity, -management, -inundation) %>% 
  distinct() %>% 
  pivot_longer(starts_with("m"), names_to = "mw_type", values_to = "mw_value")
ggplot(test) +
  geom_boxplot(aes(x = reorder(veg_code, mw_value, mean), y = -mw_value))


DBI::dbDisconnect(watina)

#Bereken per meetpunt het aantal hydrojaren waarin bepaalde representatieve periode wel of niet wordt overschreden
tijdreeks %>% 
  group_by(loc_code, HydroJaar) %>%
  summarise(max_reprper = max(ReprPeriode)) %>% 
  mutate(reprper_gr = ifelse(max_reprper > 30, 1, 0)) %>% 
  group_by(loc_code, reprper_gr) %>% 
  count()
