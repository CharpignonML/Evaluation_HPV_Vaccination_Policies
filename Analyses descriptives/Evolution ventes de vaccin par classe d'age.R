library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

## Données SNDS
df_snds <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_hpv_dose_classeage_sex_anneemois.csv")
df_snds <- df_snds |>
  mutate(
    Date  = as.Date(paste(annee_vaccin, mois_vaccin, "01", sep = "-"), format = "%Y-%m-%d"),
    total = rowSums(across(c(nb_vaccins_dose1, nb_vaccins_dose2, nb_vaccins_dose3)), na.rm = TRUE)
  ) |>
  select(Date, classe_age, total)

# Regroupement des tranches d'âge en 3 catégories
df_snds <- df_snds %>%
  mutate(categorie_age = case_when(
    classe_age == "11-14" ~ "11-14",
    classe_age == "15-19" ~ "15-19",
    classe_age == "20-26" ~ "20-26",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(categorie_age))

df_snds_age <- df_snds %>%
  group_by(Date, categorie_age) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

couleurs <- c("11-14" = "#EE82EE", "15-19" = "#9400D3", "20-26" = "#4B0082")
df_snds_age$categorie_age <- factor(df_snds_age$categorie_age, 
                                    levels = c("11-14", "15-19", "20-26"))

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

ggplot(df_snds_age, aes(x = Date, y = total, color = categorie_age)) +
  
  geom_vline(xintercept = df_labels$Date, linetype = "dashed", color = "grey3", linewidth = 0.5) +
  geom_text(
    data = df_labels,
    aes(x = Date, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90, hjust = 1.05, vjust = -0.3,
    size = 4, color = "grey3"
  ) +
  
  geom_line() +
  
  scale_color_manual(values = couleurs, name = "Tranche d'âge") +
  scale_y_continuous(name = "Nombre de ventes de vaccins") +
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
  ggtitle("Nombre de ventes de vaccin HPV au cours du temps par tranche d'âge")
