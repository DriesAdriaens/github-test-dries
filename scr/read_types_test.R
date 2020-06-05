library(tidyverse)
library(n2khab)

read_types() %>% distinct(groundw_dep)

read_scheme_types() %>% filter(scheme == "GW_05.1_terr") %>% distinct(typegroup_shortname)

read_scheme_types() %>% 
  filter(scheme == "GW_05.1_terr") %>% 
  write.csv("C:/Users/dries_adriaens/Documents/test.csv")

read_schemes() %>% view()

read_env_pressures() %>% view()
