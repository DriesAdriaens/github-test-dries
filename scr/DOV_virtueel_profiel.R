library(tidyverse)
library(inbodb)
library(sf)
library(assertthat)
library(dplyr)
library(tibble)
library(stringr)
library(ggpattern)

# Uitleg ----

# Via onderstaande scripts maak je grafieken van de (hydro)geologische opbouw van de ondergrond. Je haalt de basisgegevens hiervoor uit de export van het resultaat van de "virtueel profiel" tool in de DOV-verkenner (csv). Je moet je profiel dus in de DOV-verkenner tekenen. Het bestand geeft je de diktes per stratum. Mogelijk dien je in je csv de "," nog om te zetten in "." (bv. in notepad++ > replace). Er zijn afzonderlijke scripts per gebruikt model: geologisch (G3DV3.1) dan wel hydrogeologisch (H3Dv2); en per detailniveau: resp. geologische formatie of lid, en hydrogeologische hoofd- of subeenheid. De keuze hangt af van waar je in geïnteresseerd bent.
# Als input heb je dus de export nodig uit de "virtueel profiel" tool. Daarnaast ook de (kleur)legende die bij de verschillende strata hoort.
# Aangezien de export enkel de diktes aangeeft, moet je de absolute hoogten zelf instellen. Niet echt optimaal, maar zo is het. Hiervoor dien je in het script ergens de absolute hoogte van het hoogste punt in het profiel in te vullen. Die dien je in de "virtueel profiel" tool bepalen. De modellen gebruiken immers elk een ander DTM, dat bovendien afwijkt van het DTMVLII, en niet enkel in resolutie (100 m). 


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

# G3Dv3.1 Formatie  -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_fm <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_fm_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/chart_fm_164585_167343_172859_167329.csv",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 88.5,
         Val_max_taw = Val_max_rel - Max_all + 88.5
         ) %>% 
  inner_join(leg %>% filter(klasse == "fm"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_fm) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam),
              # outline.type = "full"
              ) +
  coord_cartesian(
    ylim = c(-100,100)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  #                    ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "fm") %>%
                      inner_join(prof_data_fm %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Formatie",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +
  

# G3Dv3.1 Lid -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_ld <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_ld_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/chart_ld_164585_167343_172859_167329.csv",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 88.5,
         Val_max_taw = Val_max_rel - Max_all + 88.5
         ) %>% 
  inner_join(leg %>% filter(klasse == "ld"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_ld) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam),
              # outline.type = "full"
  ) +
  coord_cartesian(
    ylim = c(-100,100)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "ld") %>%
                      inner_join(prof_data_ld %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Lid",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")


# H3Dv2 Hoofdeenheid -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_he <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_he_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/chart_he_164585_167343_172859_167329.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_H_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 88.5,
         Val_max_taw = Val_max_rel - Max_all + 88.5
         ) %>% 
  inner_join(leg %>% filter(klasse == "he"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_he) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_he %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "he") %>%
                      inner_join(prof_data_he %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Hoofdeenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +


# H3Dv2 Subeenheid -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_se <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/chart_se_164585_167343_172859_167329.csv",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 88.5,
         Val_max_taw = Val_max_rel - Max_all + 88.5
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
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "se") %>%
                      inner_join(prof_data_se %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
                    ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Subeenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +



# H3Dv2 Basiseenheid -----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_be <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/DOV_virtual_profile_Dijle_1_be_164548_170031_172752_170087.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_B_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 88.5,
         Val_max_taw = Val_max_rel - Max_all + 88.5
  ) %>% 
  inner_join(leg %>% filter(klasse == "be"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_be) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_be %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_data_be %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +



# Dijlevallei ----

# We werken hier met de informatie voor de hydrogeologische basiseenheid omdat het H3Dv2 model een ruimer gebied afdekt dan G3Dv31 blijkbaar. Anders ontbreekt de informatie voor het kleine deel op Waals grondgebied in het zuiden van het studiegebied.

# H3Dv2 Basiseenheid -----

## Transect 1 (uiterst noord) ----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_be_1 <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/DOV_virtual_profile_Dijle_1_be_164548_170031_172752_170087.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_B_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 87.7,
         Val_max_taw = Val_max_rel - Max_all + 87.7
  ) %>% 
  inner_join(leg %>% filter(klasse == "be"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_be_1) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_be_1 %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_data_be_1 %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R_2/255, G_2/255, B_2/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +

## Transect 2 ----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_be_2 <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/DOV_virtual_profile_Dijle_2_be_164585_167343_172859_167329.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_B_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 85,
         Val_max_taw = Val_max_rel - Max_all + 85
  ) %>% 
  inner_join(leg %>% filter(klasse == "be"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_be_2) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_be_2 %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_data_be_2 %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R_2/255, G_2/255, B_2/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +

## Transect 3 ----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_be_3 <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/DOV_virtual_profile_Dijle_3_be_164548_165047_172864_165019.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_B_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 95.39,
         Val_max_taw = Val_max_rel - Max_all + 95.39
  ) %>% 
  inner_join(leg %>% filter(klasse == "be"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_be_3) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_be_3 %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_data_be_3 %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R_2/255, G_2/255, B_2/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +


## Transect 4 (uiterst zuiden) ----

# Restructure data export from DOV tool "Virtueel profiel" frm "DOV Verkenner" (csv, decimal point, semicolon separated), for "formatie" level
prof_data_be_4 <- read.table(
  # "C:/Users/dries_adriaens/Downloads/chart_se_27520_176270_237520_175990.csv",
  "C:/Users/dries_adriaens/Downloads/DOV_virtual_profile_Dijle_4_be_164716_162107_172920_162135.csv",
  sep = ";", dec = ".", header = TRUE) %>% 
  pivot_longer(cols = -c("Lijnafstand..km.", "INV"),
               values_to = "Value",
               names_to = "Layer",
               # names_to = c("Model", "Type", "Nr"),
               names_prefix = "hcovv2_B_",
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
         # Val_min_taw = Val_min_rel - Max_all + 96.95,
         # Val_max_taw = Val_max_rel - Max_all + 96.95,
         Val_min_taw = Val_min_rel - Max_all + 94.9,
         Val_max_taw = Val_max_rel - Max_all + 94.9
  ) %>% 
  inner_join(leg %>% filter(klasse == "be"), by = c("Layer" = "nr"))

# Make area plots at "formatie" level, with "official" geological color legend
ggplot(prof_data_be_4) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_data_be_4 %>% filter(Aquitard == 1),
                      aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, group = alt_naam),
                      pattern_color = NA, pattern_fill = "black", fill = NA, pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal", pattern_spacing = 0.01,
                      pattern_res = 300) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_data_be_4 %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R_2/255, G_2/255, B_2/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    # guide = guide_legend(ncol = 7)
  ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")# +


## Compilatie ----


prof_comp <- prof_data_be_1 %>% mutate(profnr = 1) %>% 
  bind_rows(prof_data_be_2 %>% mutate(profnr = 2)) %>% 
  bind_rows(prof_data_be_3 %>% mutate(profnr = 3)) %>% 
  bind_rows(prof_data_be_4 %>% mutate(profnr = 4))

ggplot(prof_comp) +
  geom_ribbon(aes(x = Dist/10, ymin = Val_min_taw, ymax = Val_max_taw, fill = alt_naam)) +
  geom_ribbon_pattern(data = prof_comp %>% filter(Aquitard == 1),
                      aes(x = Dist/10, ymin = Val_min_taw, ymax = Val_max_taw, 
                          group = alt_naam,
                          ),
                      pattern_color = NA,
                      pattern_fill = "black",
                      fill = NA,
                      pattern_density = 0.1,
                      pattern_angle = 45, pattern_orientation = "horizontal",
                      # pattern_spacing = 0.01,
                      # pattern_res = 300
                      ) +
  coord_cartesian(
    ylim = c(-100,100),
    # xlim = c(125,175)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-2000, 500, 25),
  #                    # minor_breaks = seq(-1000, 500, 5)
  # ) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "be") %>%
                      inner_join(prof_comp %>% distinct(alt_naam), by = "alt_naam") %>%
                      mutate(
                        colorhex = rgb(R_2/255, G_2/255, B_2/255)
                      ) %>%
                      pull(colorhex, alt_naam),
                    labels = function(x) str_wrap(x, width = 20),
                    guide = guide_legend(
                      # ncol = 2,
                      label.theme = element_text(size = 8, lineheight = 0.8))
  ) +
  facet_wrap(~ paste("Transect nr.", profnr),
             # nrow = 4
             ) +
  labs(x = "Afstand (km)", y = "mTAW",
       # fill = "Basiseenheid H3Dv2",
       fill = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        text = element_text(size = 9), axis.title = element_text(size = 9))# +

ggsave("test_prof_dyl.png", 
       # width = 9, height = 18,
       width = 16, height = 16,
       dpi = 300, units = "cm")