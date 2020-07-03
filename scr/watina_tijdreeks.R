library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


watina <- connect_watina()

#tabel met peilmetingen opvragen

tijdreeks <-
  tbl(watina, "vwDimMeetpunt") %>% 
  # filter(MeetpuntCode %in% c("KASP001", "KASP002", "KASP032")) %>%
  filter(MeetpuntCode %in% c("SILP001", "SILP002", "SILP003", "SILP004")) %>%
  inner_join(tbl(watina, "vwDimPeilpunt") %>%
               select(-contains("Openbaarheid"),
                      -contains("PeilpuntWID"),
                      -contains("Naam")) %>%
               distinct() %>%
               mutate(mvTAW = ReferentieNiveauTAW - ReferentieNiveauMaaiveld) %>%
               group_by(MeetpuntWID) %>%
               summarise(mvTAW = mean(mvTAW)),
             by = "MeetpuntWID") %>%
  inner_join(tbl(watina, "FactPeilMeting"), by = "MeetpuntWID") %>% 
  inner_join(tbl(watina, "vwDimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  collect

ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum), y = mMaaiveld, colour = paste(MeetpuntCode,
                                                               " (",
                                                               round(mvTAW,2),
                                                               " mTAW)"))) +
# non clipping zoom
  # coord_cartesian(ylim = c(-1, 0.1)
  #                 , xlim = c(ymd("2009-01-01"), ymd("2017-01-01"))
  #                 ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
# clipping zoom
  # xlim(ymd("2009-01-01"), ymd("2017-01-01")) +
  # ylim(-1, 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv", colour = "Meetpunt")

tijdreeks %>% collect %>% View()

unique(tijdreeks$MeetpuntCode)
tijdreeks %>% distinct(MeetpuntCode)

tbl(watina, "vwDimMeetpunt") %>% 
  collect %>% 
  filter(grepl("SIL", MeetpuntCode))

tbl(watina, "vwFactPeilMeting") %>% distinct(PeilmetingStatusCode)

DBI::dbDisconnect(watina)
