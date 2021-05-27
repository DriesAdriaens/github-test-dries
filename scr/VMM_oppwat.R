library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

temp0 <- readxl::read_xlsx("Q:/Prjdata/Users/dries_adriaens/FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per jaar", range = "A8:J14495")

temp <- temp0 %>% 
  filter((Symbool == "NO3-" & Berekening == "Percentiel90")|
           (Symbool == "oPO4" & Berekening == "Gemiddelde"),
         Meetpunt != 220900) %>% 
  mutate(Symbool = ifelse(Symbool == "NO3-", "N-NO3", "P-PO4"),
         Meetpunt_naam = ifelse(Meetpunt == "220500", "Korbeek-Dijle", "St-Joris-Weert"),
         Jaar_datum = ymd(paste0(Jaar, "0601"))) %>% 
  group_by(Meetpunt, Symbool) %>%
  arrange(Meetpunt, Symbool, Jaar) %>% 
  mutate(
    diff = ifelse(is.na(Jaar - lag(Jaar)), 0, Jaar - lag(Jaar)),
    cumsum_dif = cumsum(diff > 1),
    grp_diff = as.numeric(Meetpunt) + percent_rank(cumsum_dif)/10)

ggplot(temp,
       aes(x = Jaar, y = Waarde, colour = Meetpunt_naam)) +
  geom_point() +
  geom_line(aes(group = grp_diff)) +
  facet_wrap(~Symbool, scales = "free_y") +
  labs(y = "mg/L", colour = "Meetpunt") +
  theme_bw()


temp2 <- readxl::read_xlsx("Q:/Prjdata/Users/dries_adriaens/FysicochemischeMetingenPerMeetpuntJaarParameterBerekening_Dijle.xlsx", sheet = "Metingen per datum", range = "A8:I20542")

temp2 <- temp2 %>% 
  filter((Symbool == "NO3-" | Symbool == "oPO4"),
         Meetpunt != 220900) %>% 
  mutate(Symbool = ifelse(Symbool == "NO3-", "N-NO3", "P-PO4"),
         Meetpunt_naam = ifelse(Meetpunt == "220500", "Korbeek-Dijle", "St-Joris-Weert"))

ggplot(temp2,
       aes(x = Datum, y = Waarde, colour = Meetpunt_naam)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Symbool, scales = "free_y") +
  theme_bw() +
  labs(y = "mg/L", colour = "Meetpunt")

ggplot() +
  geom_point(data = temp2, aes(x = ymd(Datum), y = Waarde, colour = Meetpunt_naam), alpha = 0.1) +
  # geom_point(data = temp, aes(x = Jaar_datum, y = Waarde, colour = Meetpunt_naam)) +
  geom_line(data = temp,
            aes(x = Jaar_datum, y = Waarde, colour = Meetpunt_naam, group = grp_diff), size = 1) +
  facet_wrap(~Symbool, scales = "free_y") +
  labs(y = "mg/L", colour = "Meetpunt", x = "Datum") +
  theme_bw()
