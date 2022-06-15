library(tidyverse)
library(inbodb)
library(sf)
library(assertthat)
library(dplyr)
library(tibble)
library(stringr)

tmp <- read.table("C:/Users/dries_adriaens/Downloads/chart_fm.csv", sep = ";", dec = ".", header = TRUE) %>% 
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
         Value_neg = -Value
         ) %>% 
  select(-INV) %>% 
  group_by(Layer) %>% 
  mutate(sel = max(Value),
         Layer_min = min(Value)) %>% 
  ungroup() %>% 
  filter(sel > 0) %>% 
  group_by(Dist) %>% 
  mutate(Tot_Dist = sum(Value),
         Layer_Max_Dist = max(Layer)) %>% 
  ungroup() %>% 
  mutate(Tot_Max = max(Tot_Dist),
         Tot_Min = min(Tot_Dist),
         Value_new = ifelse(Layer == Layer_Max_Dist, Value - Tot_Max, Value),
         # Value = Value - Tot_Max
         )

ggplot(tmp) +
  geom_area(aes(x = Dist, y = Value, fill = as.factor(Layer))) +
  # coord_cartesian(
  #   ylim = c(150,250)
  # ) +
  scale_x_continuous(expand = c(0,0))# +
  # theme(legend.position = "bottom", legend.direction = "horizontal")# +
  # scale_fill_manual(values = )






leg <- readxl::read_xlsx("C:/GDRIVE/GIS/Geologie/kleurcode_Tertiair_aanp100322_def.xlsx", sheet = "eigen_tabel")
leg %>% 
  filter(klasse == "fm") %>%
  # select() %>%
  # arrange(theme) %>%
  # distinct() %>%
  mutate(
    colorhex = rgb(R/255, G/255, B/255)
  ) %>% 
  pull(colorhex, nr)
  

tmp2 <- read.table("C:/Users/dries_adriaens/Downloads/chart_fm.csv", sep = ";", dec = ".", header = TRUE) %>% 
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
  # arrange(Dist, -Layer) %>% 
  mutate(Sum_Dist = sum(Value),
         Val_cumsum = cumsum(Value),
         Val_min = lag(Val_cumsum, default = 0),
         Val_max = Val_min + Value,
         Val_min_rel = Sum_Dist - Val_max,
         Val_max_rel = Sum_Dist - Val_min) %>% 
  ungroup() %>% 
  mutate(Max_all = max(Sum_Dist),
         Val_min_taw = Val_min_rel - Max_all - 36,
         Val_max_taw = Val_max_rel - Max_all - 36) %>% 
  inner_join(leg %>% filter(klasse == "fm"), by = c("Layer" = "nr"))


ggplot(tmp2) +
  geom_ribbon(aes(x = Dist, ymin = Val_min_taw, ymax = Val_max_taw, fill = code),
              # outline.type = "full"
              ) +
  coord_cartesian(
    ylim = c(-200,0)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = leg %>%
                      filter(klasse == "fm") %>%
                      inner_join(tmp2 %>% distinct(code), by = "code") %>%
                      mutate(
                        colorhex = rgb(R/255, G/255, B/255)
                      ) %>%
                      pull(colorhex, code))# +
  # theme(legend.position = "bottom", legend.direction = "horizontal")# +
  

            