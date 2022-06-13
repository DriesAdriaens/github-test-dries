library(watina)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(inbodb)
library(tibble)

basic_statistics <- function (watina = connect_watina(),
                              area_code = NULL,
                              startdate = NULL,
                              enddate = paste(day(today()), month(today()), year(today()))) {
  variables = c("CondF", "pHF", "HCO3","P-PO4", "N-NO2", "N-NO3", "N-NH4",
                "SO4", "Cl", "Na", "K", "Ca", "Mg", "Fe")
  watina <- connect_watina()
  locs=get_locs(watina, area_codes = area_code, loc_type = "P")
  chemdata=get_chem(watina,
                    locs=locs,
                    startdate = startdate,
                    en_range = c(-0.1, 0.1),
                    enddate = enddate) %>%
    filter (chem_variable %in% variables) %>% collect
  sub_ph_cond = chemdata %>%
    filter(.$chem_variable == "CondF" |.$chem_variable == "pHF")
  sub_anion_cation = chemdata %>% 
    filter(!.$chem_variable == "CondF" |.$chem_variable == "pHF") %>%
    filter (between(elneutr, -0.1, 0.1)) %>% rbind(sub_ph_cond) %>% 
    group_by(loc_code,date,lab_sample_id,chem_variable) %>% 
    summarise(value = mean(value)) %>%
    ungroup %>% 
    select(chem_variable,value) %>% 
    group_by(chem_variable) %>% 
    summarise(max=max(value),
              `90-percentiel`= quantile(value,0.9),
              mean=mean(value),
              `10-percentiel`= quantile(value,0.1),
              min = min(value),
              observations=n()) %>% 
    column_to_rownames('chem_variable')  %>%
    t() %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column() %>% 
    select(rowname,CondF, pHF, HCO3,`P-PO4`, `N-NO2`, `N-NO3`, `N-NH4`, SO4, Cl, Na, K, Ca, Mg, Fe) %>%
    rename(EC25 = CondF, Fetot = Fe, statistics = rowname) %>% 
    mutate_at(vars(EC25), ~(round(., 0))) %>%
    mutate_at(vars(-c(EC25,statistics)), ~(round(., 2)))
  sub_anion_cation}

test <- basic_statistics(area_code = "MOM",
                 startdate = "01/01/1997",
                 enddate = "31/12/2022"
                 )

test_chem <- get_chem(watina, 
         locs = get_locs(watina, area_codes = "MOM", loc_type = "P"),
         startdate = "01/01/1997",
         enddate = "31/12/2022") %>% 
  collect()

count(test_chem %>% filter(chem_variable == "N-NO2"), loc_code, date) %>% View()
