library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


watina <- connect_watina()

#tabel met peilmetingen opvragen

tijdreeks <-
  tbl(watina, "vwDimMeetpunt") %>% 
  filter(MeetpuntCode %in% c("SILP003", "SILP002", "SILP001")) %>%
  inner_join(tbl(watina, "FactPeilMeting"), by = "MeetpuntWID") %>% 
  inner_join(tbl(watina, "vwDimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  collect

ggplot(tijdreeks) +
  geom_line(aes(x = date(Datum), y = mMaaiveld, colour = MeetpuntCode)) +
  scale_y_continuous(limits = c(-0.75, 0.1)) +
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

DBI::dbDisconnect(watina)
