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
                     loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP033", "GGVP032"),
                     loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     filterdepth_range = c(0, 5),
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
  labs(x = "Datum", y = "m-mv", colour = NULL) +
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  geom_point(data = tijdreeks %>%
               filter(ReprPeriode > 30),
             aes(x = date(Datum), y = mMaaiveld))
  ## accentueren van specifieke tijdreeks
  # geom_line(data = tijdreeks %>%
  #               filter(loc_code == "ASHP013"),
  #             aes(x = date(Datum), y = mMaaiveld),
  #             colour = "black", size = 1.5)
  
plot_tijdreeks

xg3 <- get_xg3(locs = locaties, con = watina,
                startyear = 1900, endyear = 2020,
                vert_crs = "local") %>%
  eval_xg3_series(xg3_type = c("L", "H", "V"), max_gap = 0, min_dur = 2)

ghg <- xg3$ser_mean[xg3$loc_code == "GGVP032" & xg3$xg3_variable == "hg3_lcl"]
glg <- xg3$ser_mean[xg3$loc_code == "GGVP032" & xg3$xg3_variable == "lg3_lcl"]

plot_tijdreeks + 
  geom_hline(size = 1, linetype = "dashed", aes(yintercept = ghg, color = "GHG/GLG GGVP032")) +
  geom_hline(size = 1, linetype = "dashed", aes(yintercept = glg, color = "GHG/GLG GGVP032")) +
  scale_color_discrete() +
  theme(text = element_text(size = 16)
        # , legend.position = c(0.85, 0.15)
        )

ggsave("figuur2.jpg")
ggsave("figuur2.svg")

# 3. Referentietabel NICHE inlezen ----

reftab <- read.csv("C:/Users/dries_adriaens/Documents/niche_vegetation.csv")

ggplot(reftab %>% 
         select(-nutrient_level, - acidity, -management, -inundation) %>% 
         distinct()) +
  geom_linerange(aes(x = factor(veg_code), ymin = -mhw_min, ymax = -mhw_max,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  # geom_boxplot(data = reftab %>%
  #                select(-nutrient_level, - acidity, -management, -inundation) %>%
  #                distinct() %>%
  #                pivot_longer(starts_with("m"),
  #                             names_to = "mw_type",
  #                             values_to = "mw_value"),
  #              aes(x = factor(veg_code), y = -mw_value),
  #              alpha = 0.5) +
  labs(x = "Vegetatietype", y = "bereik ghg (cm-mv)", colour = "bodemtype") +
  ylim(c(-300, 50))

ggplot(reftab %>% 
         select(-nutrient_level, - acidity, -management, -inundation) %>% 
         distinct()) +
  geom_linerange(aes(x = factor(veg_code), ymin = -mlw_min, ymax = -mlw_max,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  # geom_boxplot(data = reftab %>%
  #                select(-nutrient_level, - acidity, -management, -inundation) %>%
  #                distinct() %>%
  #                pivot_longer(starts_with("m"),
  #                             names_to = "mw_type",
  #                             values_to = "mw_value"),
  #              aes(x = factor(veg_code), y = -mw_value),
  #              alpha = 0.5) +
  labs(x = "Vegetatietype", y = "bereik glg (cm-mv)", colour = "bodemtype") +
  ylim(c(-300, 50))

ggplot() +
  geom_boxplot(data = reftab %>%
                 select(-nutrient_level, - acidity, -management, -inundation) %>% 
                 distinct() %>% 
                 pivot_longer(starts_with("m"), 
                              names_to = "mw_type", 
                              values_to = "mw_value") %>% 
                 mutate(mxw = regmatches(mw_type, regexpr("m.w", mw_type)),
                        minmax = str_sub(mw_type, 5, 7)), 
               aes(x = factor(veg_code), y = -mw_value/100), alpha = 1) +
  # geom_boxplot(data = reftab %>%
  #                select(-nutrient_level, - acidity, -management, -inundation) %>%
  #                distinct() %>%
  #                pivot_longer(starts_with("m"),
  #                             names_to = "mw_type",
  #                             values_to = "mw_value") %>%
  #                mutate(mxw = regmatches(mw_type, regexpr("m.w", mw_type)),
  #                       minmax = str_sub(mw_type, 5, 7)),
  #              aes(x = factor(veg_code), y = -mw_value/100, colour = mxw),
  #              alpha = 0) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  labs(x = "Vegetatietype", y = "Referentiebereik GXG (m-mv)") + 
  geom_hline(aes(yintercept = ghg, color = "GHG/GLG GGVP032"),
             size = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = glg, color = "GHG/GLG GGVP032"),
             size = 1, linetype = "dashed") +
  # scale_color_manual(name = NULL,
  #                    values = c("GHG/GLG GGVP032" = "purple")) +
  theme(text = element_text(size = 16),
        # legend.position = c(0.9,0.95),
        legend.position = "bottom",
        legend.justification = c(1,0),
        legend.key.width = unit(1, "cm"))
ggsave("figuur3.jpg")

ggplot() +
  geom_boxplot(data = reftab %>%
                 select(-nutrient_level, - acidity, -management, -inundation) %>% 
                 distinct() %>% 
                 pivot_longer(starts_with("m"), 
                              names_to = "mw_type", 
                              values_to = "mw_value"), 
               aes(x = reorder(veg_code, mw_value, median), y = -mw_value/100)) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  labs(x = "Vegetatietype", y = "Referentiebereik GXG (m-mv)")

ggplot() +
  geom_linerange(data = reftab %>% 
                   select(-nutrient_level, - acidity, -management, -inundation) %>% 
                   distinct(),
                 aes(x = factor(veg_code), ymin = -mlw_min/100, ymax = -mlw_max/100,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  geom_linerange(data = reftab %>% 
                   select(-nutrient_level, - acidity, -management, -inundation) %>% 
                   distinct(),
                 aes(x = factor(veg_code), ymin = -mhw_min/100, ymax = -mhw_max/100,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1) +
  geom_boxplot(data = reftab %>%
                 select(-nutrient_level, - acidity, -management, -inundation) %>% 
                 distinct() %>% 
                 pivot_longer(starts_with("m"), 
                              names_to = "mw_type", 
                              values_to = "mw_value"), 
               aes(x = reorder(veg_code, mw_value, median), y = -mw_value/100),
               alpha = 0.5) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  labs(x = "Vegetatietype", y = "Referentiebereik GXG (m-mv)", colour = "Bodemtype") +
  theme(text = element_text(size = 16))
ggsave("figuur3_bis.jpg")


#Bereken per meetpunt het aantal hydrojaren waarin bepaalde representatieve periode wel of niet wordt overschreden
tijdreeks %>% 
  group_by(loc_code, HydroJaar) %>%
  summarise(max_reprper = max(ReprPeriode)) %>% 
  mutate(reprper_gr = ifelse(max_reprper > 30, 1, 0)) %>% 
  group_by(loc_code, reprper_gr) %>% 
  count()


## 4. tijdreeks figuren TAW ----

locaties_TAW <- get_locs(watina,
                     # area_codes = "GGV",
                     loc_type = c("P", "S"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP032", "GGVP033",
                                 "GGVS003","GGVS004", "GGVS010", "GGVS011"),
                     loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     filterdepth_range = c(0, 5),
                     # filterdepth_guess = FALSE,
                     # filterdepth_na = FALSE,
                     # obswells = FALSE,
                     # obswell_aggr = c("latest", "latest_fd", "latest_sso", "mean"),
                     collect = FALSE)
locaties_TAW %>% collect() %>% View()

tijdreeks_TAW <-
  locaties_TAW %>% 
  rename(mvTAW = soilsurf_ost) %>% 
  inner_join(tbl(watina, "FactPeilMeting"), by = c("loc_wid" = "MeetpuntWID")) %>% 
  inner_join(tbl(watina, "DimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  dplyr::collect()

# 5. figuur van tijdreeks(en) voor gewenste meetpunten, TAW ----


plot_tijdreeks_TAW <- ggplot(tijdreeks_TAW) +
  geom_line(aes(x = date(Datum),
                # y = mMaaiveld,
                y = mTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
  )) +
  # geom_line(data = tijdreeks_TAW %>% filter(loc_typecode == "P"),
  #           aes(x = date(Datum), y = mvTAW,
  #               colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")),
  #           linetype = "dashed", size = 1) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1),
  #                 xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2018-01-01"), ymd("2020-08-01")) +
  # ylim(-1, 0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "mTAW", colour = "Meetpunt") +
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  geom_point(data = tijdreeks_TAW %>%
               filter(ReprPeriode > 30),
             aes(x = date(Datum), y = mTAW)) +
  theme(text = element_text(size = 16))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_TAW



DBI::dbDisconnect(watina)