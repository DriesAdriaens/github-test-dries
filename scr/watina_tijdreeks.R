library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


watina <- connect_watina()

# 1. tabel met peilmetingen opvragen voor gewenste meetpunten ----

locaties <- get_locs(watina,
                     # area_codes = "SIL",
                     # loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     # loc_vec = c("KASP001", "KASP002", "KASP032"),
                     loc_vec = c("SILP001", "SILP002", "SILP003", "SILP004"),
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
  inner_join(tbl(watina, "vwDimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  collect

# 2. figuur van tijdreeks(en) voor gewenste meetpunten ----

ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum), y = mMaaiveld,
                colour = paste(loc_code, " (", round(mvTAW,2), " mTAW)"))) +
  # non-clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1)
  #                 , xlim = c(ymd("2009-01-01"), ymd("2017-01-01"))
  #                 ) +
  # end non-clipping zoom
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  # clipping zoom
  # xlim(ymd("2009-01-01"), ymd("2017-01-01")) +
  # ylim(-1, 0.1) +
  # end clipping zoom
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = "Meetpunt")


DBI::dbDisconnect(watina)
