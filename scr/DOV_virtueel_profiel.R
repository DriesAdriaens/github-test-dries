library(tidyverse)
library(inbodb)
library(sf)
library(assertthat)
library(dplyr)
library(tibble)
library(stringr)
library(ggpattern)

# Read (color) legend for both geological levels "formatie" (fm) and "lid" (ld), with information on code, chronostratigraphy
leg <- readxl::read_xlsx("C:/GDRIVE/GIS/Geologie/G3Dv31/ModeleenhedenG3Dv3_1.xlsx", sheet = "leg_unique") %>% 
  mutate(alt_code = gsub("_", ", ", code),
         alt_naam = ifelse(klasse == "fm",
                           paste("Formatie van", naam),
                           paste("Lid van", naam)),
         alt_naam = case_when(
           klasse == "fm" ~ paste("Formatie van", naam),
           klasse == "ld" ~ paste("Lid van", naam),
           TRUE ~ paste(code, "-", naam)
         )
         )

# Formatie G3Dv3 -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_fm <- read.table(
  "C:/Users/dries_adriaens/Downloads/chart_fm.csv",
  # "C:/Users/dries_adriaens/Downloads/chart_fm_turnhout.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "g3dv3_F_",
               # names_sep = "_"
  ) %>% 
  rename(Dist = Lijnafstand..km.) %>% 
  mutate(Layer = as.numeric(Layer),
         Dist = Dist/10,
         ) %>% 
  select(-INV) %>% 
  group_by(Layer) %>% 
  mutate(sel = max(Value),
         ) %>% 
  ungroup() %>% 
  filter(sel > 0) %>%
  group_by(Dist) %>% 
  mutate(Sum_Dist = sum(Value),
         Val_cumsum = cumsum(Value),
         Val_min = lag(Val_cumsum, default = 0),
         Val_max = Val_min + Value,
         Val_min_rel = Sum_Dist - Val_max,
         Val_max_rel = Sum_Dist - Val_min) %>% 
  ungroup() %>% 
  mutate(Max_all = max(Sum_Dist),
         Val_min_taw = Val_min_rel - Max_all + 76.14,
         Val_max_taw = Val_max_rel - Max_all + 76.14
         # Val_min_taw = Val_min_rel - Max_all + 33.22,
         # Val_max_taw = Val_max_rel - Max_all + 33.22
  ) %>% 
  inner_join(leg %>% filter(klasse == "fm"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_fm) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam),
              # outline.type = "full"
              ) +
  coord_cartesian(
    ylim = c(-150,100)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-2000, 500, 25),
                     # minor_breaks = seq(-1000, 500, 5)
                     ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "fm") %>%
                      inner_join(prof_data_fm %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW", fill = "Formatie")# +
  # theme(legend.position = "bottom", legend.direction = "horizontal")# +
  

# Lid G3Dv3 -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_ld <- read.table(
  "C:/Users/dries_adriaens/Downloads/chart_lid.csv",
  # "C:/Users/dries_adriaens/Downloads/chart_fm_turnhout.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "g3dv3_L_",
               # names_sep = "_"
  ) %>% 
  rename(Dist = Lijnafstand..km.) %>% 
  mutate(Layer = as.numeric(Layer),
         Dist = Dist/10,
  ) %>% 
  select(-INV) %>% 
  group_by(Layer) %>% 
  mutate(sel = max(Value),
  ) %>% 
  ungroup() %>% 
  filter(sel > 0) %>%
  group_by(Dist) %>% 
  mutate(Sum_Dist = sum(Value),
         Val_cumsum = cumsum(Value),
         Val_min = lag(Val_cumsum, default = 0),
         Val_max = Val_min + Value,
         Val_min_rel = Sum_Dist - Val_max,
         Val_max_rel = Sum_Dist - Val_min) %>% 
  ungroup() %>% 
  mutate(Max_all = max(Sum_Dist),
         Val_min_taw = Val_min_rel - Max_all + 76.14,
         Val_max_taw = Val_max_rel - Max_all + 76.14
         # Val_min_taw = Val_min_rel - Max_all + 33.22,
         # Val_max_taw = Val_max_rel - Max_all + 33.22
  ) %>% 
  inner_join(leg %>% filter(klasse == "ld"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_ld) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam),
              # outline.type = "full"
  ) +
  coord_cartesian(
    ylim = c(-150,100)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-2000, 500, 25),
                     # minor_breaks = seq(-1000, 500, 5)
  ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "ld") %>%
                      inner_join(prof_data_ld %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW", fill = "Lid")# +
# theme(legend.position = "bottom", legend.direction = "horizontal")# +


# Subeenheid H3Dv2 -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_se <- read.table(
  "C:/Users/dries_adriaens/Downloads/chart_se.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_S_",
               # names_sep = "_"
  ) %>% 
  rename(Dist = Lijnafstand..km.) %>% 
  mutate(Layer = as.numeric(Layer),
         # Dist = Dist/10,
  ) %>% 
  select(-INV) %>% 
  group_by(Layer) %>% 
  mutate(sel = max(Value),
  ) %>% 
  ungroup() %>% 
  filter(sel > 0) %>%
  group_by(Dist) %>% 
  mutate(Sum_Dist = sum(Value),
         Val_cumsum = cumsum(Value),
         Val_min = lag(Val_cumsum, default = 0),
         Val_max = Val_min + Value,
         Val_min_rel = Sum_Dist - Val_max,
         Val_max_rel = Sum_Dist - Val_min) %>% 
  ungroup() %>% 
  mutate(Max_all = max(Sum_Dist),
         Val_min_taw = Val_min_rel - Max_all + 96.95,
         Val_max_taw = Val_max_rel - Max_all + 96.95
        ) %>% 
  inner_join(leg %>% filter(klasse == "se"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_se) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_se %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-2000, 500, 25),
                     # minor_breaks = seq(-1000, 500, 5)
  ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "se") %>%
                      inner_join(prof_data_se %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Subeenheid H3Dv2",
       fill = NULL) +
theme(legend.position = "bottom", legend.direction = "horizontal")# +
            