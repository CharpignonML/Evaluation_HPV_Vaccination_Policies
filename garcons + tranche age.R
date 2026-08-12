library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

df <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_hpv_dose_classeage_sex_anneemois.csv")

df <- df %>%
  mutate(Date = as.Date(paste(annee_vaccin, mois_vaccin, "01", sep = "-"),
                        format = "%Y-%m-%d"))

# df garcons 11 - 14 ans 

df_g1114 <- df %>%
  filter(sex == "0") %>% 
  filter(classe_age == "11-14") %>%
  rowwise() %>%
  mutate(n = sum(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)                           

write_csv(df_g1114, "/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_garcons_11_14.csv")

# df uniquement 15 - 19 ans 

df_g1519 <- df %>%
  filter(sex == "0") %>% 
  filter(classe_age == "15-19") %>%
  rowwise() %>%
  mutate(n = sum(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)  

write_csv(df_g1519, "/Users/charlottederogis/Desktop/Stage/R/discontinuités/table_snds_garcons_garçons_15_19.csv")

# df uniquement 20 - 26 ans 

df_g2026 <- df %>%
  filter(sex == "0") %>% 
  filter(classe_age == "20-26") %>%
  rowwise() %>%
  mutate(n = sum(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)  

write_csv(df_g2026, "/Users/charlottederogis/Desktop/Stage/R/discontinuités/table_snds_garcons_garçons_20_26.csv")

# df uniquement > 26 ans 

dfg26 <- df %>%
  filter(sex == "0") %>% 
  filter(classe_age %in% c("27-32" , "33-37", "38-42", "43-47", "48-52")) %>%
  rowwise() %>%
  mutate(n = sum(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)  

write_csv(dfg26, "/Users/charlottederogis/Desktop/Stage/R/discontinuités/table_snds_garcons_plus26.csv")

# df 11 - 19 ans 

dfg1119 <- df %>%
  filter(sex == "0") %>% 
  filter(classe_age %in% c("11-14" , "15-19")) %>%
  rowwise() %>%
  mutate(n = sum(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)  

write_csv(dfg1119, "/Users/charlottederogis/Desktop/Stage/R/discontinuités/table_snds_garcons_11_19.csv")
