library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


watina <- connect_watina()

#tabel met peilmetingen opvragen

tijdreeks <-
  tbl(watina, "vwDimMeetpunt") %>% 
  filter(MeetpuntCode %in% c("KASP001", "KASP002", "KASP032")) %>%
  # filter(MeetpuntCode %in% c("SILP001", "SILP002", "SILP003", "SILP004")) %>%
  inner_join(tbl(watina, "FactPeilMeting"), by = "MeetpuntWID") %>% 
  inner_join(tbl(watina, "vwDimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  collect

ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum), y = mMaaiveld, colour = MeetpuntCode)) +
  coord_cartesian(ylim = c(-3, 0)) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  geom_hline(yintercept = 0, colour = "black", size = 1, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.x = element_line(linetype = "dashed")) +
  labs(x = "Datum", y = "m-mv")

tijdreeks %>% collect %>% View()

unique(tijdreeks$MeetpuntCode)
tijdreeks %>% distinct(MeetpuntCode)

tbl(watina, "vwDimMeetpunt") %>% 
  collect %>% 
  filter(grepl("SIL", MeetpuntCode))

tbl(watina, "vwFactPeilMeting") %>% distinct(PeilmetingStatusCode)

DBI::dbDisconnect(watina)
