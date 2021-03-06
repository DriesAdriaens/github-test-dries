library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(inbodb)


watina <- connect_watina()
# watina <- connect_inbo_dbase("W0002_00_Watina", autoconvert_utf8 = TRUE)

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     # area_codes = "GGV",
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     # loc_vec = c("VLVP001", "VLVP002", "VLVP003", "VLVP004", "VLVP005", "VLVP006"), #transectNZ Vloethemveld, enkel ondiepe peilbuizen
                     # loc_vec = c("VLVP001", "VLVP301", "VLVP004", "VLVP304"), #transectNZ Vloethemveld, enkel peilbuiskoppels
                     # loc_vec = c("VLVP007", "VLVP008", "VLVP009", "VLVP010", "VLVP003"), #transectOW Vloethemveld
                     # loc_vec = c("BZKP001", "BZKP002"),
                     # loc_vec = c("VUIP001"),
                     # loc_vec = c("TILP001", "TILP002", "TILP003", "TILP004", "TILP005"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     # loc_vec = c("VRIP008", "VRIP009", "VRIP010", "VRIP011", "VRIP012", "VRIP013"),
                     # loc_vec = c(
                     #   "VRIP045",
                     #   "VRIP028",
                     #   "VRIP031",
                     #   "VRIP033",
                     #   "VRIP046",
                     #   "VRIP047"
                     #   ),
                     # loc_vec =  c("DYLP004", "DYLP017", "DYLP006"),
                     # loc_vec =  c("DYLP012", "DYLP013", "DYLP056"),
                     # loc_vec = c("DYLP090", "DYLP090", "DYLP089", "DYLP089", "DYLP084", "DYLP081", "DYLP073", "DYLP057", "DYLP012", "DYLP018", "DYLP034", "DYLP019", "DYLP018", "DYLP016", "DYLP009", "DYLP005", "DYLP017", "DYLP002", "DYLP001", "DYLP074", "DYLP044", "DYLP004", "DYLP039", "DYLP091", "DYLP045"),
                     # loc_vec = c("KASP032", "KASP030", "KASP031", "KASP001", "KASP002"),
                     loc_vec = c("SBRP005", "SBRP006"),
                     # loc_vec = c("VRIP003", "VRIP006"
                     #             #, "VRIP007"
                     #             ),
                     # loc_vec = c("VRIP004", "VRIP005"),
                     # loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     loc_validity = c("VLD"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     filterdepth_range = c(0, 100),
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
  dplyr::collect() %>% 
  group_by(loc_code) %>%
  arrange(loc_code, Datum) %>% 
  mutate(
    # reprp01 = ifelse(ReprPeriode > 60, 1, 0),
    # reprp01_ = ifelse(reprp01 == 0 & lag(reprp01) == 1, 1, reprp01),
    # reprp01_ = ifelse(is.na(reprp01_), reprp01, reprp01_),
    # cumsum = cumsum(reprp01_),
    # rnk_pct = percent_rank(cumsum),
    # grp = loc_wid + rnk_pct/10,
    # diff = Datum - lag(Datum),
    diff = ifelse(is.na(ymd(Datum) - lag(ymd(Datum))), ReprPeriode, ymd(Datum) - lag(ymd(Datum))),
    cumsum_dif = cumsum(diff > 60),
    grp_diff = loc_wid + percent_rank(cumsum_dif)/10,
    ) %>% 
  ungroup()

# tijdreeks %>%
#   select(loc_code, Datum, mMaaiveld, mTAW, ReprPeriode, diff, cumsum_dif, grp_diff) %>%
#   View()


# 2. figuur van tijdreeks(en) voor gewenste meetpunten ----


plot_tijdreeks <- ggplot(tijdreeks
                         # %>% 
                         #   filter(loc_code %in% c("VRIP031", "VRIP045", "VRIP033"))
                         ) +
  geom_line(aes(x = date(Datum),
                y = mMaaiveld,
                # y = mTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                group = grp_diff
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
                )) +
  ## non-clipping zoom
  coord_cartesian(
    # ylim = c(-1, 0.1),
    xlim = c(ymd("2010-01-01"), ymd("2022-01-01"))
  ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  # xlim(ymd("2012-01-01"), ymd("2021-01-01")) +
  # ylim(-0.85, 0.05) +
  geom_hline(yintercept = 0, colour = "black",
             # size = 1,
             # linetype = "dotted"
             ) +
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom") +
  labs(x = "Datum", y = "m-mv", colour = NULL) +
  ## plotten van peilmetingen als punten
  geom_point(#data = tijdreeks,
             aes(x = date(Datum), y = mMaaiveld,
                 colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")),
             size = 1)
  ## punten plotten met reprper > limiet
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
  ## accentueren van specifieke tijdreeks
  # geom_line(data = tijdreeks %>%
  #               filter(loc_code == "ASHP013"),
  #             aes(x = date(Datum), y = mMaaiveld),
  #             colour = "black", size = 1.5)

# ggsave("Grondwaterdynamiek_tijdreeksen.png", plot_tijdreeks, dpi = 300,
#        width = 15,
#        height = 10,
#        units = "cm")
plot_tijdreeks


xg3 <- get_xg3(locs = locaties, con = watina,
                startyear = 1900, endyear = 2021,
                vert_crs = "local",
               with_estimated = TRUE,
               truncated = TRUE,
               collect = TRUE
               )

xg3_eval_series <- eval_xg3_series(xg3, xg3_type = c("L", "H", "V"), max_gap = 100, min_dur = 1)

xg3_eval_avail <- eval_xg3_avail(xg3, xg3_type = c("L", "H", "V"))

ghg <- xg3_eval_series$ser_mean[xg3_eval_series$loc_code == "GGVP032" & xg3_eval_series$xg3_variable == "hg3_lcl"]
glg <- xg3_eval_series$ser_mean[xg3_eval_series$loc_code == "GGVP032" & xg3_eval_series$xg3_variable == "lg3_lcl"]

# xg3 %>% 
#   group_by(loc_code) %>% 
#   summarise(across(everything(), ~ length(.x[!is.na(.x)]))) %>% 
#   write.table("clipboard", sep="\t", row.names=FALSE)

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
         # filter(veg_code %in% c(14, 17, 18)) %>% 
         distinct()) +
  geom_linerange(aes(x = factor(veg_code),
                     ymin = -mhw_min, ymax = -mhw_max,
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
  geom_hline(yintercept = 0) +
  labs(x = "Vegetatietype", y = "bereik ghg (cm-mv)", colour = "bodemtype")

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

ggplot(reftab %>% 
         select(-nutrient_level, - acidity, -management, -inundation) %>% 
         filter(veg_code %in% c(14, 17, 18)) %>% 
         distinct()) +
  geom_linerange(aes(#x = factor(veg_code),
    x = veg_type,
    ymin = -mhw_min, ymax = -mhw_max,
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
  geom_hline(yintercept = 0) +
  labs(x = "Vegetatietype", y = "bereik ghg (cm-mv)", colour = "bodemtype")# +
#ylim(c(-300, 50))

reftab_long <- reftab %>% 
  select(-nutrient_level, - acidity, -management, -inundation) %>% 
  distinct() %>% 
  pivot_longer(starts_with("m"), 
               names_to = "mw_var", 
               values_to = "mw_value") %>% 
  mutate(mw_type = ifelse(grepl("^mh", mw_var), "GHG", "GLG"),
         mw_minmax = ifelse(grepl("min$", mw_var), "min", "max")) %>% 
  select(-mw_var) %>% 
  pivot_wider(names_from = mw_minmax, values_from = mw_value)

ggplot(reftab_long %>%
         filter(veg_code %in% 
                  # c(14, 17, 18)
                c(2, 4))) +
  geom_linerange(aes(#x = factor(veg_code),
    x = veg_type,
    ymin = -min, ymax = -max,
    colour = soil_name),
    position = position_dodge(0.5),
    size = 1) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ mw_type) +
  labs(x = "Vegetatietype", y = "waterpeil (cm-mv)", colour = "bodemtype")
ggsave("NICHE_sel_91EO_vmva.jpeg", width = 15, height = 7, units = "cm")

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


## 4. tijdreeks voor figuren TAW ----

locaties_TAW <- get_locs(watina,
                     # area_codes = "GGV",
                     loc_type = c("P", "S"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     # loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP032", "GGVP033",
                     loc_vec = c(
                       "VRIP045",
                       "VRIP028",
                       "VRIP031",
                       "VRIP033",
                       "VRIP046",
                       "VRIP047"
                     ),
                     #             "GGVS003","GGVS004", "GGVS010", "GGVS011"),
                     # loc_vec = c("VRIP003", "VRIP006", "VRIP007"),
                     loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     filterdepth_range = c(0, 10),
                     # filterdepth_guess = FALSE,
                     # filterdepth_na = FALSE,
                     # obswells = FALSE,
                     # obswell_aggr = c("latest", "latest_fd", "latest_sso", "mean"),
                     collect = FALSE)
# locaties_TAW %>% collect() %>% View()

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
  coord_cartesian(
    ylim = c(7.25, 8.25),
    xlim = c(ymd("2015-01-01"), ymd("2016-01-01"))
  ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  # xlim(ymd("2015-01-01"), ymd("2016-01-01")) +
  # ylim(-1, 0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom") +
  labs(x = "Datum", y = "mTAW", colour = "Meetpunt")
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  # geom_point(data = tijdreeks_TAW %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mTAW)) +
  # theme(text = element_text(size = 16))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_TAW



DBI::dbDisconnect(watina)
