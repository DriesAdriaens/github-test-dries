library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


watina <- connect_watina()

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     # area_codes = "KRG",
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     # loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
                     loc_vec = c("SILP004"),
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

ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum), y = mMaaiveld,
                colour = paste0(loc_code, " (", round(mvTAW,2), " mTAW)")
                # Peilmetingstatus weergeven in legende (lijntype; ingegeven/gevalideerd)
                # ,linetype = PeilmetingStatus
                )) +
  ## non-clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1)
  #                 , xlim = c(ymd("1999-01-01"), ymd("2010-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  ## clipping zoom
  # xlim(ymd("2009-01-01"), ymd("2017-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = "Meetpunt") +
  ## plotten van peilmetingen met representatieve periode > 30 dagen
  geom_point(data = tijdreeks %>%
               filter(ReprPeriode > 30),
             aes(x = date(Datum), y = mMaaiveld, alpha = ReprPeriode)) +
  scale_alpha_continuous(breaks = c(31, 60, 90, 120),
                         labels = c("31", "60", "90", ">120"),
                         limits = c(31, 120)) + labs(alpha = "ReprPer > 30 (dagen)")
  ## accentueren van specifieke tijdreeks
  # geom_line(data = tijdreeks %>%
  #               filter(loc_code == "ASHP013"),
  #             aes(x = date(Datum), y = mMaaiveld),
  #             colour = "black", size = 1.5)


DBI::dbDisconnect(watina)

#Bereken per meetpunt het aantal hydrojaren waarin bepaalde representatieve periode wel of niet wordt overschreden
tijdreeks %>% 
  group_by(loc_code, HydroJaar) %>%
  summarise(max_reprper = max(ReprPeriode)) %>% 
  mutate(reprper_gr = ifelse(max_reprper > 30, 1, 0)) %>% 
  group_by(loc_code, reprper_gr) %>% 
  count()
