library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

setwd("G:/Mijn Drive/Projecten/PRJ_SBO_Future_Floodplains/140_RawData/Systeembeschrijvingen/Dijle/")

Watkwal_agg <- readxl::read_xlsx("FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per jaar", range = "A8:J14495")

Watkwal_agg_sel <- Watkwal_agg %>% 
  filter((Symbool == "NO3-" & Berekening == "Percentiel90")|
           (Symbool == "oPO4" & Berekening == "Gemiddelde")|
           (Symbool == "KjN" & Berekening == "Percentiel90"),
         Meetpunt != 220900) %>% 
  mutate(
    BKN = case_when(
      Symbool == "NO3-" ~ 5.65,
      Symbool == "oPO4" ~ 0.14,
      Symbool == "KjN" ~ 6),
    Symbool = case_when(
      Symbool == "NO3-" ~ "N-NO3",
      Symbool == "oPO4" ~ "P-PO4",
      Symbool == "KjN" ~ "N-KjN",
      TRUE ~ Symbool),
    Meetpunt_naam = ifelse(Meetpunt == "220500", "220500; Korbeek-Dijle", "221000; St-Joris-Weert"),
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
  geom_line(aes(x = Jaar, y = BKN, linetype = Symbool), colour = "black", size = 1) +
  geom_point() +
  geom_line(aes(group = grp_diff)) +
  facet_wrap(~Symbool, scales = "free_y") +
  labs(y = "mg/L", colour = "Meetpunt", linetype = "Basismilieunorm") +
  scale_x_continuous(minor_breaks = seq(1900,2050,2), limits = c(1989, 2021)) +
  theme_bw() + 
  theme(legend.position = "bottom")
ggsave("Dijle_Waterkwaliteit_VMM_agg.png", dpi = 300, width = 15, height = 10, units = "cm")


Watkwal <- readxl::read_xlsx("FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per datum", range = "A8:I20542")

Watkwal_sel <- Watkwal %>% 
  filter((Symbool == "NO3-" | Symbool == "oPO4"),
         Meetpunt != 220900) %>% 
  mutate(Symbool = ifelse(Symbool == "NO3-", "N-NO3", "P-PO4"),
         Meetpunt_naam = ifelse(Meetpunt == "220500", "220500; Korbeek-Dijle", "221000; St-Joris-Weert"))

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
