library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

## Donées gt sur les dates du SNDS

df_gt <- read.csv2("/Users/charlottederogis/Desktop/Stage/pret\ GitHub/ggt\ total\ dates\ snds.csv")

df_gt$Date <- as.Date(paste0(df_gt$Date, "-01"), format = "%Y-%m-%d")

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

ggplot(df_gt, aes(x = Date, y = n)) +
  
  geom_vline(xintercept = df_labels$Date, linetype = "dashed", color = "grey3", linewidth = 0.5) +
  geom_text(
    data = df_labels,
    aes(x = Date, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90, hjust = 1.05, vjust = -0.3,
    size = 4, color = "grey3"
  ) +
  
  geom_line() +
  
  scale_y_continuous(name = "Intensity of HPV related researches") +
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
  ggtitle("Evolution of the intensity of HPV related researches")
