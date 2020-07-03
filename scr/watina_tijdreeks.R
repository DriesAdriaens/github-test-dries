library(watina)
library(dplyr)


watina <- connect_watina()

#tabel met peilmetingen opvragen
tbl(watina, "vwDimMeetpunt") %>% 
  filter(MeetpuntCode == "SILP003") %>% 
  inner_join(tbl(watina, "vwFactPeilMeting"), by = "MeetpuntWID") %>% 
  inner_join(tbl(watina, "vwDimTijd"), by = c("TijdWID" = "DatumWID")) %>% 
  collect %>% 
  View()

DBI::dbDisconnect(watina)