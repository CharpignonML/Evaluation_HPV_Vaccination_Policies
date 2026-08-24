library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

## Donées SNDS

df_snds <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_hpv_dose_age.csv")

df_snds$periode <- as.Date(paste0("01/", df_snds$periode), format = "%d/%m/%Y")

df_snds <- df_snds %>%
  mutate(total = rowSums(across(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3)), na.rm = TRUE))

df_snds <- df_snds %>% 
  select(periode, sex, total)

df_snds_sex <- df_snds %>%
  group_by(periode, sex) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

df_snds_sex <- df_snds_sex %>%
  mutate(categorie_sex = case_when(
    sex == "0" ~ "Garçons/Hommes",
    sex == "1" ~ "Filles/Femmes",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(categorie_sex))

couleurs <- c("Garçons/Hommes" = "#4EB265", "Filles/Femmes" = "#DC050C")
df_snds_sex$categorie_sex <- factor(df_snds_sex$categorie_sex, 
                                    levels = c("Garçons/Hommes", "Filles/Femmes"))

# dates clés + labels
labels_dates <- c(
  "2007-03-09" = "Reco filles 11-19",
  "2013-01-15" = "Avis HCSP âge",
  "2016-05-02" = "Reco HSH <26",
  "2017-09-13" = "AMM gardasil9",
  "2019-12-19" = "Parution reco garçons 11-19",
  "2020-12-04" = "Application reco garçons 11-19",
  "2022-05-07" = "Vaccination par pharmaciens",
  "2023-02-28" = "Annonce campagnes collèges",
  "2023-10-02" = "Début campagnes collèges",
  "2025-12-12" = "Âge étendu à 26 ans",
  "2020-03-01" = "Confinement covid", 
  "2011-07-15" = "Polémique 1", 
  "2013-11-15" = "Polémique SEP"
)

df_labels <- data.frame(
  Date  = as.Date(names(labels_dates)),
  label = unname(labels_dates)
)

ggplot(df_snds_sex, aes(x = periode, y = total, color = categorie_sex)) +
  
  geom_vline(xintercept = df_labels$Date, linetype = "dashed", color = "grey3", linewidth = 0.5) +
  geom_text(
    data = df_labels,
    aes(x = Date, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90, hjust = 1.05, vjust = -0.3,
    size = 4, color = "grey3"
  ) +
  
  geom_line() +
  
  scale_color_manual(values = couleurs, name = "Sexe") +
  scale_y_continuous(name = "Number of HPV vaccine dispensing") +
  scale_x_date(date_breaks = "1 year", date_labels = "%m/%Y") +
  coord_cartesian(clip = "off") +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(angle = 90, vjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 60, r = 10, b = 10, l = 10)
  ) +
  ggtitle("Evolution of HPV vaccine dispensing by sex")
