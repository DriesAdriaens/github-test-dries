library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)


watina <- connect_watina()

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     area_codes = "MOM",
                     loc_type = c("P"),
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     # loc_vec = c("GGVP011", "GGVP013", "GGVP015", "GGVP017", "GGVP033", "GGVP032"),
                     loc_validity = c("VLD", "ENT"),
                     # mask = NULL,
                     # join_mask = FALSE,
                     # buffer = 10,
                     filterdepth_range = c(0, 30),
                     # filterdepth_guess = FALSE,
                     # filterdepth_na = FALSE,
                     # obswells = FALSE,
                     # obswell_aggr = c("latest", "latest_fd", "latest_sso", "mean"),
                     collect = FALSE)

locaties %>% collect() %>% View()

# 2. opdeling naar raai ----


trans_mom_1 <- c("MOMP005", "MOMP006", "MOMP007", "MOMP008", "MOMP009")
trans_mom_2 <- c("MOMP001", "MOMP003", "MOMP004", "MOMP010", "MOMP002",
                 "MOMP011", "MOMP029", "MOMP012", "MOMP013","MOMS001",
                 "MOMS009", "MOMP014",
                 "MOMP015", "MOMP016", "MOMS008")
trans_mom_3 <- c("MOMP027", "MOMS011", "MOMP026", "MOMP025", "MOMP024",
                 "MOMP023", "MOMS007", "MOMS010", "MOMP017", "MOMP018",
                 "MOMP019", "MOMP020", "MOMP021", "MOMP022")
trans_mom_4 <- c("MOMP028")

transects <- data.frame(loc_code = locaties$loc_code,
                        transect_nr = 0, stringsAsFactors = FALSE) %>% 
  mutate(transect_nr = ifelse(loc_code %in% trans_mom_1, 1,
                              ifelse(loc_code %in% trans_mom_2, 2,
                                     ifelse(loc_code %in% trans_mom_3, 3,
                                            ifelse(loc_code %in% trans_mom_4, 4,
                                                   transect_nr)))))

locaties <- locaties %>% 
  left_join(transects, by = "loc_code")

# 3. tijdreeks aanmaken ----

tijdreeks <-
  locaties %>% 
  rename(mvTAW = soilsurf_ost) %>% 
  inner_join(tbl(watina, "FactPeilMeting"), by = c("loc_wid" = "MeetpuntWID")) %>% 
  inner_join(tbl(watina, "DimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  dplyr::collect()

tijdreeks <- tijdreeks %>% 
  left_join(transects, by = "loc_code")

# 4. figuur van tijdreeks(en) voor gewenste meetpunten ----

# 4.1 raai 1 ----

plot_tijdreeks_trans_mom_1 <- ggplot(tijdreeks %>% 
                           filter(transect_nr == 1)) +
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
  xlim(ymd("2017-01-01"), ymd("2020-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = NULL) 
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_trans_mom_1

check <- tijdreeks %>% 
  filter(transect_nr == 1 & mMaaiveld > 0) %>% 
  arrange(loc_code, date(Datum))
  
# 4.2 raai 2 ----

plot_tijdreeks_trans_mom_2 <- ggplot(tijdreeks %>% 
                                       filter(transect_nr == 2)) +
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
  xlim(ymd("2017-01-01"), ymd("2020-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = NULL) 
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_trans_mom_2

# 4.3 raai 3 ----

plot_tijdreeks_trans_mom_3 <- ggplot(tijdreeks %>% 
                                       filter(transect_nr == 3)) +
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
  xlim(ymd("2017-01-01"), ymd("2020-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = NULL) 
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_trans_mom_3

# 4.4 raai 4 ----

plot_tijdreeks_trans_mom_4 <- ggplot(tijdreeks %>% 
                                       filter(transect_nr == 4)) +
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
  xlim(ymd("2017-01-01"), ymd("2020-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = NULL) 
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  # geom_point(data = tijdreeks %>%
  #              filter(ReprPeriode > 30),
  #            aes(x = date(Datum), y = mMaaiveld))
## accentueren van specifieke tijdreeks
# geom_line(data = tijdreeks %>%
#               filter(loc_code == "ASHP013"),
#             aes(x = date(Datum), y = mMaaiveld),
#             colour = "black", size = 1.5)

plot_tijdreeks_trans_mom_4

