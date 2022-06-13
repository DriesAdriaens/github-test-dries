library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(inbodb)


watina <- connect_watina()
# watina <- connect_inbo_dbase("W0002_00_Watina", autoconvert_utf8 = TRUE)
# watina_prd <- connect_inbo_dbase("D0025_00_watina", autoconvert_utf8 = TRUE)
# tbl(watina_prd, "vw_Peilmeting") %>% 
#   filter(MeetpuntCode == "GGVP015", year(PeilmetingDatum) >= 2021)

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     # area_codes = "ELS",
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     loc_vec = c("MOMP023", "MOMP024", "MOMP025", "MOMP026", "MOMP027"),
                     # loc_vec = c("VLVP001", "VLVP002", "VLVP003", "VLVP004", "VLVP005", "VLVP006"), #transectNZ Vloethemveld, enkel ondiepe peilbuizen
                     # loc_vec = c("VLVP001", "VLVP301", "VLVP004", "VLVP304"), #transectNZ Vloethemveld, enkel peilbuiskoppels
                     # loc_vec = c("VLVP007", "VLVP008", "VLVP009", "VLVP010", "VLVP003"), #transectOW Vloethemveld
                     # Grote Gete Meetraai 4
                     # loc_vec = c("GGVP001", "GGVP003", "GGVP005", "GGVP006", "GGVP007", "GGVP037", "GGVP038", "GGVP039"),
                     # Grote Gete Meetraai 3
                     # loc_vec = c("GGVP008", "GGVP009", "GGVP010", "GGVP011", "GGVP012", "GGVP013", "GGVP014", "GGVP015", "GGVP016", "GGVP032", "GGVP033", "GGVP017"
                     # ,"GGVP034", "GGVP035", "GGVP036",
                     # , "GGVS003", "GGVS004", "GGVS010", "GGVS011"
                     # ),
                     # Grote Gete Meetraai 2
                     # loc_vec = c("GGVP018", "GGVP019", "GGVP020","GGVP021", "GGVP022", "GGVP023", "GGVP024", "GGVP025"),
                     # Grote Gete Meetraai 1
                     # loc_vec = c("GGVP026", "GGVP027", "GGVP028", "GGVP029", "GGVP030", "GGVP031"),
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
                     # loc_vec = c("SBRP005", "SBRP006"),
                     # loc_vec = c("VRIP003", "VRIP006"
                     #             #, "VRIP007"
                     #             ),
                     # loc_vec = c("VRIP004", "VRIP005"),
                     loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     # loc_validity = c("VLD"),
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
  filter(#PeilmetingStatus == "Gevalideerd",
         PeilmetingStatus %in% c("Gevalideerd", "Ingegeven"),
         is.na(PeilmetingCategorie)
         ) %>% 
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
  ungroup() %>% 
  mutate(
    test = ifelse(Datum < "2007-01-01", "2004-2005", "2018-2021")
  )

# tijdreeks %>%
#   select(loc_code, Datum, mMaaiveld, mTAW, ReprPeriode, diff, cumsum_dif, grp_diff) %>%
#   View()


# 2. xg3's opvragen en gxg's berekenen (zeer ruw nu, met zeer liberale eisen naar minimumduur (1 jaar) en tussenliggende ontbrekende jaren (100 jaar) ----

xg3 <- get_xg3(locs = locaties, con = watina,
               startyear = 1900, endyear = 2021,
               vert_crs = "local",
               with_estimated = TRUE,
               truncated = TRUE,
               collect = TRUE
)
xg3_long <- xg3 %>% 
  pivot_longer(cols = c("lg3_lcl", "hg3_lcl", "vg3_lcl"), values_to = "xg3_val", names_to = "xg3")


xg3_eval_series <- eval_xg3_series(xg3, xg3_type = c("L", "H", "V"), max_gap = 100, min_dur = 1)

# xg3_eval_avail <- eval_xg3_avail(xg3, xg3_type = c("L", "H", "V"))

xg3_long <- xg3_long %>% 
  left_join(xg3_eval_series, by = c("loc_code" = "loc_code", "xg3" = "xg3_variable"))

# ggplot(xg3_long 
#        %>% filter(hydroyear %in% (2018:2020),
#                   xg3 != "vg3_lcl")
# ) +
#   # geom_line(aes(x = hydroyear,
#   #               y = ser_mean,
#   #               colour = loc_code,
#   #               linetype = xg3)) +
#   geom_line(aes(x = hydroyear,
#                 y = xg3_val,
#                 colour = loc_code,
#                 linetype = xg3)) +
#   geom_point(aes(x = hydroyear,
#                  y = xg3_val,
#                  colour = loc_code,
#                  linetype = xg3)) +
#   # geom_step(direction = "mid") +
#   scale_linetype_manual(values = c("lg3_lcl" = "solid", "hg3_lcl" = "dashed", "vg3_lcl" = "dotted")) +
#   scale_x_continuous(breaks = c(2018, 2019, 2020), minor_breaks = NULL) +
#   scale_colour_brewer(
#     # type = "div", palette = 7,
#     # type = "div", palette = 8,
#     type = "div", palette = 9
#   )


# 3. figuur van tijdreeks(en), in m-mv, voor gewenste meetpunten ----

## 3.1 zonder gxg's ----

### zonder facets

plot_tijdreeks_mv <- ggplot(tijdreeks
                         %>%
                         #   filter(loc_code %in% c("VRIP031", "VRIP045", "VRIP033"),
                         # filter(mMaaiveld >= 0)
                         filter(loc_typecode == "P")
                         ) +
  geom_line(aes(x = date(Datum),
                y = mMaaiveld,
                # y = mTAW,
                # colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                colour = loc_code,
                group = grp_diff
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
                )) +
  ## plotten van peilmetingen als punten
  # geom_point(#data = tijdreeks,
  #   aes(x = date(Datum),
  #       # y = mMaaiveld,
  #       y = mTAW,
  #       # colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
  #       colour = loc_code),
  #   size = 1) +
  ## non-clipping zoom
  # coord_cartesian(
  #   ylim = c(-2.5, 1.5),
  #   # xlim = c(ymd("2021-06-13"), ymd("2021-08-18"))
  # ) +
  ## clipping zoom
  # xlim(ymd("2021-06-13"), ymd("2021-08-18")) +
  # ylim(-0.85, 0.05) +
  scale_x_date(
    date_breaks = "year",
               date_labels = "%Y",
               date_minor_breaks = "3 months",
               # limits = c(ymd("2017-01-01"), ymd("2021-01-01"))
               ) +
  scale_colour_brewer(
    # type = "div", palette = 7,
    # type = "div", palette = 8,
    type = "div", palette = 9,
    # type = "qual", palette = "Set1",
    # type = "qual", palette = "Accent",
    # type = "qual", palette = "Dark2"
    ) +
  geom_hline(yintercept = 0, colour = "black",
             # size = 1,
             # linetype = "dotted"
             ) +
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # text = element_text(size = 20),
        ) +
  labs(x = "Datum", y = "m-mv", colour = NULL) +
  ## opgedeelde figuur
  # facet_wrap(~test, scales = "free_x",) #+
  facet_grid(~test, space = "free_x", scales = "free_x")
  ## punten plotten met reprper > limiet
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
  ## accentueren van specifieke tijdreeks
  # geom_line(data = tijdreeks %>%
  #               filter(loc_code == "ASHP013"),
  #             aes(x = date(Datum), y = mMaaiveld),
  #             colour = "black", size = 1.5)
  

# ggsave("Grondwaterdynamiek_tijdreeksen_GGV_raai.png", plot_tijdreeks, dpi = 300,
#        width = 15,
#        height = 10,
#        units = "cm")
plot_tijdreeks_mv


### met facets

plot_tijdreeks_mv_fac <- ggplot(tijdreeks) +
  geom_hline(yintercept = 0, colour = "black", size = 0.5) +
  geom_line(aes(x = date(Datum),
                y = mMaaiveld,
                colour = loc_code,
                # colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                group = grp_diff
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(32.25, 34.25),
  # #                 xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2017-01-01"), ymd("2022-03-01")) +
  # ylim(-1, 0.1) +
  facet_wrap(~loc_code) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        # legend.position = "bottom"
  ) +
  labs(x = "Datum", y = "m-mv", colour = NULL)

plot_tijdreeks_mv_fac



##3.2 met gxg's ----

tijdreeks <- tijdreeks %>% 
  left_join(xg3_eval_series %>%
              filter(xg3_variable != "combined") %>% 
              pivot_wider(id_cols = loc_code, values_from = ser_mean, names_from = xg3_variable),
            by = "loc_code")

### zonder facets

plot_tijdreeks_mv +
  geom_line(data = tijdreeks, aes(x = date(Datum),
                y = lg3_lcl,
                colour = loc_code),
            linetype = "solid", size = 0.3) +
  geom_line(data = tijdreeks, aes(x = date(Datum),
                y = hg3_lcl,
                colour = loc_code),
            linetype = "dashed", size = 0.3)

### met facets

plot_tijdreeks_mv_fac +
  geom_line(data = tijdreeks, aes(x = date(Datum),
                                  y = lg3_lcl,
                                  colour = loc_code),
            linetype = "solid", size = 0.3) +
  geom_line(data = tijdreeks, aes(x = date(Datum),
                                  y = hg3_lcl,
                                  colour = loc_code),
            linetype = "dashed", size = 0.3)


# ggsave("Grondwaterdynamiek_tijdreeksen_GGV_raai_gxg.png", dpi = 300,
#        width = 15,
#        height = 10,
#        units = "cm")


# ghg <- xg3_eval_series$ser_mean[xg3_eval_series$loc_code == "GGVP032" & xg3_eval_series$xg3_variable == "hg3_lcl"]
# glg <- xg3_eval_series$ser_mean[xg3_eval_series$loc_code == "GGVP032" & xg3_eval_series$xg3_variable == "lg3_lcl"]
# 
# # xg3 %>% 
# #   group_by(loc_code) %>% 
# #   summarise(across(everything(), ~ length(.x[!is.na(.x)]))) %>% 
# #   write.table("clipboard", sep="\t", row.names=FALSE)
# 
# plot_tijdreeks + 
#   geom_hline(size = 1, linetype = "dashed", aes(yintercept = ghg, color = "GHG/GLG GGVP032")) +
#   geom_hline(size = 1, linetype = "dashed", aes(yintercept = glg, color = "GHG/GLG GGVP032")) +
#   scale_color_discrete() +
#   theme(text = element_text(size = 16)
#         # , legend.position = c(0.85, 0.15)
#         )
# 
# ggsave("figuur2.jpg")
# ggsave("figuur2.svg")

# 4. figuur met tijdreeksen per jaar ----

doy <- date(c("2016-01-01",
              "2016-02-01",
              "2016-03-01",
             "2016-04-01",
             "2016-05-01",
             "2016-06-01",
             "2016-07-01",
             "2016-08-01",
             "2016-09-01",
             "2016-10-01",
             "2016-11-01",
             "2016-12-01"))

doy <- tibble(mon = month(doy, label = T),
             jul = yday(doy))

plot_jaarreeks <- ggplot(tijdreeks
                         %>%
                           filter(loc_code %in% c("MOMP027"))
                         ) +
  geom_line(aes(x = yday(date(Datum)),
                # x = Jaardag_Nummer,
                y = mMaaiveld,
                # y = mTAW,
                # colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                # colour = paste0(loc_code, "_", Jaar)
                colour = as.factor(Jaar),
                linetype = loc_code,
                # group = grp_diff
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
                ), size = 1) +
  ## non-clipping zoom
  coord_cartesian(
    # ylim = c(-1, 0.1),
    # xlim = c(ymd("2010-01-01"), ymd("2022-01-01"))
  ) +
  # scale_x_date(date_breaks = "month", date_labels = "%M") +
  scale_x_continuous(breaks = doy$jul, labels = doy$mon) +
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
        legend.position = "right",
        legend.direction = "vertical"
  ) +
  labs(x = "Datum", y = "m-mv", colour = "Jaar", linetype = "Peilbuis") +
  ## plotten van peilmetingen als punten
  geom_point(#data = tijdreeks,
    aes(x = yday(date(Datum)), y = mMaaiveld,
        # colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
        colour = as.factor(Jaar),
        linetype = loc_code),
    size = 1)
  ## opgedeelde figuur
  # facet_wrap(~test, scales = "free_x",) +
  # facet_grid(~test, space = "free_x", scales = "free_x")
## punten plotten met reprper > limiet
# geom_point(data = tijdreeks %>%
#              filter(ReprPeriode > 30),
#            aes(x = date(Datum), y = mMaaiveld))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

# ggsave("Grondwaterdynamiek_jaarreeksen.png", plot_tijdreeks, dpi = 300,
#        width = 15,
#        height = 10,
#        units = "cm")

plot_jaarreeks


# 3. Referentietabel NICHE inlezen ----

reftab <- read.csv("C:/Users/dries_adriaens/Documents/niche_vegetation.csv")
niche_nednaam <- readxl::read_xlsx("G:/Mijn Drive/Databanken/Niche/HAB_NICHE_Koppeling.xlsx", sheet = "NICHE_LEG")
colnames(niche_nednaam) <- snakecase::to_snake_case(colnames(niche_nednaam))
reftab <- reftab %>% 
  left_join(niche_nednaam, by = c("veg_code" = "verbond"))

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
  ylim(c(-300, 50)) +
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
  geom_hline(yintercept = 0) +
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

ggplot(reftab_long
       # %>%
       #   filter(!soil_name %in% c("Z1", "Z2", "ZV"
       #                            , "K1", "KV"
       #                            ))
       ) +
  geom_linerange(aes(#x = factor(veg_code),
    x = reorder(paste0(veg_code, " - ", nederlandse_naam), -veg_code),
    ymin = -min/100, ymax = -max/100,
    colour = soil_name),
    position = position_dodge(0.5),
    size = 0.5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ mw_type) +
  labs(x = "Vegetatietype", y = "waterpeil (m-mv)", colour = "bodemtype") +
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave("Systeembeschrijving_GroteGete_GXG_Niche.png", width = 20, height = 15, units = "cm")

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
  geom_linerange(data = reftab %>% 
                   select(-nutrient_level, - acidity, -management, -inundation) %>% 
                   distinct(),
                 aes(x = factor(veg_code), ymin = -mlw_min/100, ymax = -mlw_max/100,
                     colour = soil_name),
                 position = position_dodge(0.5),
                 size = 1, linetype = "dashed") +
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


# 4. tijdreeks voor figuren, in mTAW ----

locaties_TAW <- get_locs(watina,
                     # area_codes = "GGV",
                     loc_type = c("P", "S"),
                     # loc_vec = c("MOMP023", "MOMP024", "MOMP025", "MOMP026", "MOMP027"),
                     # Grote Gete Meetraai 3
                     loc_vec = c("GGVP008", "GGVP009", "GGVP010", "GGVP011", "GGVP013", "GGVP015", "GGVP016", "GGVP032", "GGVP033", "GGVP017", "GGVS003", "GGVS004", "GGVS010", "GGVS011"),
                     # loc_vec = c("GGVP034", "GGVP035", "GGVP015", "GGVP016","GGVP017", "GGVP033", "GGVP036", "GGVS010"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     # # loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP032", "GGVP033",
                     # loc_vec = c(
                     #   "VRIP045",
                     #   "VRIP028",
                     #   "VRIP031",
                     #   "VRIP033",
                     #   "VRIP046",
                     #   "VRIP047"
                     # ),
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
# locaties_TAW %>% View()

tijdreeks_TAW <-
  locaties_TAW %>%
  rename(mvTAW = soilsurf_ost) %>%
  inner_join(tbl(watina, "FactPeilMeting"), by = c("loc_wid" = "MeetpuntWID")) %>%
  inner_join(tbl(watina, "DimTijd"), by = c("TijdWID" = "DatumWID")) %>%
  filter(#PeilmetingStatus == "Gevalideerd",
    PeilmetingStatus %in% c("Gevalideerd", "Ingegeven"),
    is.na(PeilmetingCategorie)
  ) %>%
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
    grp_diff = loc_wid + percent_rank(cumsum_dif)/10
    ) %>%
  ungroup() %>%
  mutate(
    test = ifelse(Datum < "2007-01-01", "2004-2005", "2018-2021")
  )


# 2. xg3's opvragen en gxg's berekenen (zeer ruw nu, met zeer liberale eisen naar minimumduur (1 jaar) en tussenliggende ontbrekende jaren (100 jaar) ----

xg3_TAW <- get_xg3(locs = locaties_TAW, con = watina,
               startyear = 1900, endyear = 2021,
               vert_crs = "ostend",
               with_estimated = TRUE,
               truncated = TRUE,
               collect = TRUE
)
xg3_long_TAW <- xg3_TAW %>% 
  pivot_longer(cols = c("lg3_ost", "hg3_ost", "vg3_ost"), values_to = "xg3_val", names_to = "xg3")


xg3_eval_series_TAW <- eval_xg3_series(xg3_TAW, xg3_type = c("L", "H", "V"), max_gap = 100, min_dur = 1)

# xg3_eval_avail <- eval_xg3_avail(xg3, xg3_type = c("L", "H", "V"))

xg3_long_TAW <- xg3_long_TAW %>% 
  left_join(xg3_eval_series, by = c("loc_code" = "loc_code", "xg3" = "xg3_variable"))


# 5. figuur van tijdreeks(en) voor gewenste meetpunten, TAW ----


plot_tijdreeks_TAW <- ggplot(tijdreeks_TAW) +
  geom_line(aes(x = date(Datum),
                # y = mMaaiveld,
                y = mTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                group = grp_diff
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
  )) +
  # geom_line(data = tijdreeks_TAW %>% filter(loc_typecode == "P"),
  #           aes(x = date(Datum), y = mvTAW,
  #               colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")),
  #           linetype = "dashed",
  #           # size = 1
  #           ) +
  ## non-clipping zoom
  # coord_cartesian(
  #   ylim = c(25.5, 28.5),
  #   # xlim = c(ymd("2015-01-01"), ymd("2016-01-01"))
  # ) +
scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  # xlim(ymd("2015-01-01"), ymd("2016-01-01")) +
  # ylim(-1, 0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom") +
  labs(x = "Datum", y = "mTAW", colour = "Meetpunt") +
  scale_colour_brewer(
    # type = "div", palette = 7,
    # type = "div", palette = 8,
    type = "div", palette = 9,
    # type = "qual", palette = "Set1",
    # type = "qual", palette = "Accent",
    # type = "qual", palette = "Dark2"
  ) #+
# facet_grid(~test, space = "free_x", scales = "free_x")
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

## Met gxg's

tijdreeks_TAW <- tijdreeks_TAW %>% 
  left_join(xg3_eval_series_TAW %>%
              filter(xg3_variable != "combined") %>% 
              pivot_wider(id_cols = loc_code, values_from = ser_mean, names_from = xg3_variable),
            by = "loc_code")

### zonder facets

plot_tijdreeks_TAW +
  geom_line(data = tijdreeks_TAW, aes(x = date(Datum),
                                  y = lg3_ost,
                                  colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")),
            linetype = "solid", size = 0.3) +
  geom_line(data = tijdreeks_TAW, aes(x = date(Datum),
                                  y = hg3_ost,
                                  colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")),
            linetype = "dashed", size = 0.3)


## Andere mogelijkheden (recuperatie bestaande stukjes script)

#mTAW, met telkens de aanduiding van de maaiveldhoogte in stippellijn

ggplot(tijdreeks_TAW) +
  geom_line(aes(x = date(Datum),
                y = mTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)"),
                group = grp_diff
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1),
  #                 xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2017-10-01"), ymd("2022-03-01")) +
  # ylim(-1, 0.1) +
  geom_line(aes(x = date(Datum),
                y = mvTAW,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "mTAW", colour = NULL)

#mTAW, met telkens de aanduiding van de maaiveldhoogte in stippellijn, facetted

ggplot(tijdreeks_TAW) +
  geom_line(aes(x = date(Datum),
                y = mTAW,
                colour = loc_code,
                group = grp_diff
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(32.25, 34.25),
  # #                 xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2017-10-01"), ymd("2022-03-01")) +
  # ylim(-1, 0.1) +
  geom_line(aes(x = date(Datum),
                y = mvTAW,
                colour = loc_code), linetype = "dashed") +
  facet_wrap(~loc_code) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom") +
  labs(x = "Datum", y = "mTAW", colour = NULL)

#mTAW, met telkens de aanduiding van de maaiveldhoogte in stippellijn. De tijdreeksen van de waterpeilen van een te kiezen peilschaal staan in het grijs.

ggplot() +
  geom_line(data = tijdreeks_TAW %>%
              as.data.frame() %>%
              filter(loc_code %in% c("GGVS003")) %>% 
              select(-loc_code),
            aes(x = date(Datum),
                y = mTAW,
                group = grp_diff,
                linetype = "Naam waterloop"),
            alpha = 0.25) +
  geom_line(data = tijdreeks_TAW,
            aes(x = date(Datum),
                y = mTAW,
                colour = loc_code,
                group = grp_diff
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(31.5, 34.25),
                  # xlim = c(ymd("2018-01-01"), ymd("2021-01-01"))
  # ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  xlim(ymd("2017-10-01"), ymd("2022-03-01")) +
  # ylim(-1, 0.1) +
  geom_line(data = tijdreeks_TAW, 
            aes(x = date(Datum),
                y = mvTAW,
                colour = loc_code),
            linetype = "dashed") +
  facet_wrap(~loc_code) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        legend.position = "bottom", legend.box = "horizontal",
        legend.direction = "horizontal") +
  labs(x = "Datum", y = "mTAW", colour = NULL, linetype = NULL)



#6. Integratie in transectgegevens (DTM) ----

# DHM transectdata uit GIS inlezen (aangemaakt via qprof-plugin van QGIS)

transect_DHM <- readxl::read_xlsx("Q:/Prjdata/Projects/PRJ_SBO_Future_Floodplains/155_GIS_werkkaarten/Gete/Transect_GGV/Profiel.xlsx", sheet = "Profiel_DTM")
transect_waterlopen <- readxl::read_xlsx("Q:/Prjdata/Projects/PRJ_SBO_Future_Floodplains/155_GIS_werkkaarten/Gete/Transect_GGV/Profiel.xlsx", sheet = "Waterlopen")
transect_meetpunten <- readxl::read_xlsx("Q:/Prjdata/Projects/PRJ_SBO_Future_Floodplains/155_GIS_werkkaarten/Gete/Transect_GGV/Profiel.xlsx", sheet = "Meetpunten")

locaties_TAW <- locaties_TAW %>% 
  collect() %>% 
  left_join(transect_meetpunten, by = c("loc_code" = "id"))

ggplot() +
  geom_line(data = transect_DHM,
            aes(rec_id, DHMVIIDTMR)) +
  geom_point(data = transect_meetpunten,
             aes(s, z),
             colour = "red", size = 2) +
  geom_point(data = transect_waterlopen,
             aes(s, z),
             colour = "blue", size = 2) +
  labs(x = "Afstand (m)", y = "mTAW") +
  ylim(27, 33)
  

# plot van meetpuntdefinitie van grondwatermeetpunten

ggplot(locaties_TAW %>% filter(loc_typecode == "P"),
       aes(x = reorder(loc_code, 
                       # soilsurf_ost,
                       -y
       ))) +
  geom_linerange(aes(ymin = measuringref_ost - tubelength + filterlength, ymax = measuringref_ost),
                 size = 2) +
  geom_linerange(aes(ymin = measuringref_ost - tubelength,
                     ymax = measuringref_ost - tubelength + filterlength),
                 colour = "red",
                 size = 2) +
  geom_point(aes(y = soilsurf_ost), colour = "brown", size = 4) +
  # geom_text(aes(y = measuringref_ost, label = loc_code,
  #                angle = 90, hjust = 0
  #                )) +
  # geom_hline(aes(yintercept = soilsurf_ost), size = 2, colour = "orange") +
  # facet_grid(~reorder(loc_code, soilsurf_ost), space = "free_x", scales = "free_x") +
  geom_linerange(data = xg3_TAW %>% 
                   inner_join(locaties_TAW %>% collect(), by = "loc_code") %>% 
                   filter(hydroyear >= 2018, hydroyear < 2021, loc_typecode == "P"), 
                 aes(ymin = lg3_ost,
                     ymax = hg3_ost,
                     colour = factor(hydroyear)),
                 size = 2,
                 # colour = "blue",
                 position = position_dodge(width = 0.35)
                 ) +
  # geom_point(data = tijdreeks_TAW %>% 
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-02-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
  #                     ),
  #            aes(y = mTAW),
  #            size = 4, colour = "blue") +
  # geom_point(data = tijdreeks_TAW %>% 
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-10-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
  #              ),
  #            aes(y = mTAW),
  #            size = 4, colour = "orange") +
  labs(x = "Meetpunt", y = "mTAW", colour = str_wrap("[hg3-lg3] per hydrojaar", width = 15))


# Integratie met transectgegevens uit GIS, met behoud van hoogteligging uit peilpuntdefinitie (ook al geeft de geprojecteerde hoogteligging uit GIS een andere waarde)

ggplot(locaties_TAW %>%
           filter(loc_typecode == "P"),
         aes(x = s)) +
  geom_hline(aes(yintercept = 31.9), colour = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = 29), colour = "red", linetype = "dashed") +
  geom_linerange(aes(ymin = measuringref_ost - tubelength + filterlength,
                     ymax = measuringref_ost,
                     linetype = "piëzometer"),
                 size = 2) +
  geom_linerange(aes(ymin = measuringref_ost - tubelength,
                     ymax = measuringref_ost - tubelength + filterlength,
                     size = "filter"),
                 colour = "red",
                 # size = 2
  ) +
  geom_text(aes(y = measuringref_ost, label = loc_code,
                angle = 90, hjust = 0
  )) +
  # geom_hline(aes(yintercept = soilsurf_ost), size = 2, colour = "orange") +
  # facet_grid(~reorder(loc_code, soilsurf_ost), space = "free_x", scales = "free_x") +
  geom_linerange(data = xg3_TAW %>%
                   inner_join(locaties_TAW %>% collect(), by = "loc_code") %>%
                   filter(hydroyear >= 2018, hydroyear < 2021, loc_typecode == "P"),
                 aes(ymin = lg3_ost,
                     ymax = hg3_ost,
                     colour = factor(hydroyear)),
                 size = 2,
                 # colour = "blue",
                 position = position_dodge(width = 50)
  ) +
  geom_boxplot(data = tijdreeks_TAW %>% 
                 filter(loc_typecode == "S") %>% 
                 inner_join(transect_waterlopen, by = c("loc_code" = "watina")),
               aes(x = s,  group = cut_width(s, 5), y = mTAW,
                   # colour = factor(HydroJaar)
               ),
               outlier.alpha = 0.15, width = 30, size = 0.5,
               # position = position_dodge(width = 30),
               colour = "blue") +
  # geom_point(data = transect_waterlopen,
  #                       aes(s, z),
  #                       colour = "blue", size = 2) +
  geom_text(data = transect_waterlopen,
            aes(y = z-0.5, label = id), colour = "blue", angle = 90, hjust = 1) +
  geom_line(data = transect_DHM,
            aes(rec_id, DHMVIIDTMR)) +
  geom_point(aes(y = soilsurf_ost,
                 shape = "maaiveld"), colour = "brown", size = 3) +
  # geom_point(data = tijdreeks_TAW %>%
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-02-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
  #                     ),
  #            aes(y = mTAW),
  #            size = 4, colour = "blue") +
  # geom_point(data = tijdreeks_TAW %>%
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-10-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
#                     ),
#            aes(y = mTAW),
#            size = 4, colour = "orange") +
scale_y_continuous(breaks = 25:34) +
  coord_cartesian(ylim = c(25, 34)) +
  labs(x = "Afstand (m)",
       y = "mTAW", 
       colour = str_wrap("[hg3-lg3] per hydrojaar", width = 15),
       linetype = NULL,
       size = NULL,
       shape = NULL) +
  guides(linetype = guide_legend(order = 1),
         shape = guide_legend(order = 2),
         colour = guide_legend(order = 3),
         size = guide_legend(order = 4))

# Integratie met transectgegevens uit GIS, waarbij hoogteligging uit GIS gebruikt wordt i.p.v. hoogteligging uit peilpuntdefinitie, om ligging van meetpunten beter te laten aansluiten op transect

ggplot(locaties_TAW %>%
         filter(loc_typecode == "P") %>% 
         mutate(measuringref_ost = z + measuringref_ost - soilsurf_ost,
                soilsurf_ost = z),
       aes(x = s)) +
  geom_linerange(aes(ymin = measuringref_ost - tubelength + filterlength,
                     ymax = measuringref_ost,
                     linetype = "piëzometer"),
                 size = 2) +
  geom_linerange(aes(ymin = measuringref_ost - tubelength,
                     ymax = measuringref_ost - tubelength + filterlength,
                     size = "filter"),
                 colour = "red",
                 # size = 2
  ) +
  geom_text(aes(y = measuringref_ost, label = loc_code,
                angle = 90, hjust = 0
  )) +
  # geom_hline(aes(yintercept = soilsurf_ost), size = 2, colour = "orange") +
  # facet_grid(~reorder(loc_code, soilsurf_ost), space = "free_x", scales = "free_x") +
  geom_linerange(data = xg3_TAW %>%
                   inner_join(locaties_TAW %>% collect(), by = "loc_code") %>%
                   filter(hydroyear >= 2018, hydroyear < 2021, loc_typecode == "P"),
                 aes(ymin = lg3_ost,
                     ymax = hg3_ost,
                     colour = factor(hydroyear)),
                 size = 2,
                 # colour = "blue",
                 position = position_dodge(width = 50)
  ) +
  geom_boxplot(data = tijdreeks_TAW %>% 
                 filter(loc_typecode == "S") %>% 
                 inner_join(transect_waterlopen, by = c("loc_code" = "watina")),
               aes(x = s,  group = cut_width(s, 5), y = mTAW,
                   # colour = factor(HydroJaar)
               ),
               outlier.alpha = 0.15, width = 30, size = 0.5,
               # position = position_dodge(width = 30),
               colour = "blue") +
  # geom_point(data = transect_waterlopen,
  #                       aes(s, z),
  #                       colour = "blue", size = 2) +
  geom_text(data = transect_waterlopen,
            aes(y = z-0.5, label = id), colour = "blue", angle = 90, hjust = 1) +
  geom_line(data = transect_DHM,
            aes(rec_id, DHMVIIDTMR)) +
  geom_point(aes(y = soilsurf_ost,
                 shape = "maaiveld"), colour = "brown", size = 3) +
  # geom_point(data = tijdreeks_TAW %>%
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-02-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
  #                     ),
  #            aes(y = mTAW),
  #            size = 4, colour = "blue") +
  # geom_point(data = tijdreeks_TAW %>%
  #              filter(loc_typecode == "P",
  #                     Datum == ymd("2018-10-01"),
  #                     # Datum > ymd("2018-01-01"), Datum < ymd("2018-02-01")
#                     ),
#            aes(y = mTAW),
#            size = 4, colour = "orange") +
scale_y_continuous(breaks = 25:34) +
  coord_cartesian(ylim = c(25, 34)) +
  labs(x = "Afstand (m)",
       y = "mTAW", 
       colour = str_wrap("[hg3-lg3] per hydrojaar", width = 15),
       linetype = NULL,
       size = NULL,
       shape = NULL) +
  guides(linetype = guide_legend(order = 1),
         shape = guide_legend(order = 2),
         colour = guide_legend(order = 3),
         size = guide_legend(order = 4))

# ggsave("test.jpg", dpi = 300, width = 29.7, height = 10, units = "cm")

# ggplot() +
#   geom_boxplot(data = tijdreeks_TAW %>% 
#                    filter(loc_typecode == "S", HydroJaar >= 2018, HydroJaar < 2021) %>% 
#                    inner_join(transect_waterlopen, by = c("loc_code" = "watina")),
#                  aes(x = s,  group = cut_width(s, 5), y = mTAW, colour = factor(HydroJaar)),
#                outlier.shape = NA, width = 10, colour = "blue")
