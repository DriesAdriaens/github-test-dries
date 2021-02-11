library(glue)
library(DBI)
library(assertthat)
library(dplyr)
library(inbodb)
library(ggplot2)
library(inboggvegan)
library(vegan)
library(tidyr)
library(stringr)

# Make connection ----

con <- connect_inbo_dbase("D0010_00_Cydonia")

# Test INBOVEG functions of inbodb package ----

survey <- get_inboveg_survey(con, "SBO%", collect = TRUE)
record <- get_inboveg_recordings(con, "SBO%", collect = TRUE)

record %>% count(CoverageCode) %>% ggplot() + geom_bar(aes(CoverageCode, n), stat = "identity")
record %>% count(LayerCode)

header <- get_inboveg_header(con, rec_type = "classic", survey_name = "%SBO%", collect = TRUE)
header %>% count(Observer) %>% ggplot() + 
  geom_bar(aes(Observer, n), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

header %>% filter(grepl("Dries", Observer) & grepl("Siege", Observer)) %>% count(Observer)

clsf <- get_inboveg_classification(con, classif = "3130", collect = TRUE)
clsf %>% count(Name)                

clsf <- get_inboveg_classification(con)
clsf %>% count(ActionGroup)
temp <- clsf %>%
  filter(ActionGroup == "N2k") %>% 
  collect() %>% 
  count(Name) %>%
  arrange(-n) %>%
  View()
temp <- test %>% filter(ActionGroup == "N2k") %>% count(Classif) %>% collect() %>% arrange(-n)

# Load testdata from INBOVEG for CA in vegan package ----

testdata <- get_inboveg_recordings(con, "SBO%overstr%", collect = TRUE)
testdata_header <- get_inboveg_header(con, "SBO%overstr%", collect = TRUE)

## Abbreviate plant species names
Sci_uni <- testdata %>% 
  select(ScientificName) %>% 
  distinct() %>%
  mutate(temp = str_split_fixed(ScientificName, pattern = " ", n = 3),
         Sci_bin = str_c((temp[,1]), temp[,2], sep = " "),
         Sci_abbr = str_to_lower(make.cepnames(Sci_bin))) %>% 
  select(-temp) %>% 
  arrange(ScientificName)
testdata <- testdata %>% left_join(Sci_uni, by = "ScientificName")

## Transform data to wide format and replace RecordingGivid with UserReference as site name
testdata_trans <- testdata %>% 
  select(RecordingGivid, Sci_abbr, PctValue) %>% 
  spread(key = Sci_abbr, value = PctValue, fill = 0) %>% 
  left_join(testdata_header %>% 
             select(RecordingGivid, UserReference),
           by = "RecordingGivid") %>% 
  select(UserReference, everything(), -RecordingGivid) %>% 
  data.frame(row.names = "UserReference")
rownames(testdata_trans) <- testdata_trans$UserReference

## Quick vegan analysis CA
CA <- cca(testdata_trans)
ggscreeplot(CA, type = "both")
ggbiplot_vegan(CA)

CA_plot <- ordiplot(CA, type = "text", xlim = c(-4, 4), ylim = c(-2, 0.5))
identify(CA_plot, "species")
identify(CA_plot, "sites")
