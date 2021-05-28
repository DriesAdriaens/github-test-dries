library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

Watkwal_agg <- readxl::read_xlsx("G:/Mijn Drive/Projecten/PRJ_SBO_Future_Floodplains/140_RawData/Systeembeschrijvingen/Dijle/FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per jaar", range = "A8:J14495")

Watkwal_agg_sel <- Watkwal_agg %>% 
  filter((Symbool == "NO3-" & Berekening == "Percentiel90")|
           (Symbool == "oPO4" & Berekening == "Gemiddelde"),
         Meetpunt != 220900) %>% 
  mutate(Symbool = ifelse(Symbool == "NO3-", "N-NO3", "P-PO4"),
         Meetpunt_naam = ifelse(Meetpunt == "220500", "Korbeek-Dijle", "St-Joris-Weert"),
         Jaar_datum = ymd(paste0(Jaar, "0601"))) %>% 
  group_by(Meetpunt, Symbool) %>%
  arrange(Meetpunt, Symbool, Jaar) %>% 
  #berekenen van group variabele om lijngrafiek te onderbreken bij ontbrekende jaren
  mutate(
    diff = ifelse(is.na(Jaar - lag(Jaar)), 0, Jaar - lag(Jaar)),
    cumsum_dif = cumsum(diff > 1),
    grp_diff = as.numeric(Meetpunt) + percent_rank(cumsum_dif)/10)

ggplot(Watkwal_agg_sel,
       aes(x = Jaar, y = Waarde, colour = Meetpunt_naam)) +
  geom_point() +
  geom_line(aes(group = grp_diff)) +
  facet_wrap(~Symbool, scales = "free_y") +
  labs(y = "mg/L", colour = "Meetpunt") +
  theme_bw()


Watkwal <- readxl::read_xlsx("G:/Mijn Drive/Projecten/PRJ_SBO_Future_Floodplains/140_RawData/Systeembeschrijvingen/Dijle/FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per datum", range = "A8:I20542")

Watkwal_sel <- Watkwal %>% 
  filter((Symbool == "NO3-" | Symbool == "oPO4"),
         Meetpunt != 220900) %>% 
  mutate(Symbool = ifelse(Symbool == "NO3-", "N-NO3", "P-PO4"),
         Meetpunt_naam = ifelse(Meetpunt == "220500", "Korbeek-Dijle", "St-Joris-Weert"))

ggplot(Watkwal_sel,
       aes(x = Datum, y = Waarde, colour = Meetpunt_naam)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Symbool, scales = "free_y") +
  theme_bw() +
  labs(y = "mg/L", colour = "Meetpunt")

ggplot() +
  geom_point(data = Watkwal_sel, aes(x = ymd(Datum), y = Waarde, colour = Meetpunt_naam), alpha = 0.1) +
  # geom_point(data = temp, aes(x = Jaar_datum, y = Waarde, colour = Meetpunt_naam)) +
  geom_line(data = Watkwal_agg_sel,
            aes(x = Jaar_datum, y = Waarde, colour = Meetpunt_naam, group = grp_diff), size = 1) +
  facet_wrap(~Symbool, scales = "free_y") +
  labs(y = "mg/L", colour = "Meetpunt", x = "Datum") +
  theme_bw()
