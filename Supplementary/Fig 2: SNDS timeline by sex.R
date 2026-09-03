# Load libraries
library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(tidyverse)
library(scales)

# Load data

# Specify your own folder
folder_name <- "/Users/charlottederogis/Desktop/Stage/CSV populations"

# Specify your datasets (filles / garcons)
file_filles   <- paste0(folder_name, "/table_snds_filles.csv")
file_garcons  <- paste0(folder_name, "/table_snds_garcons.csv")

# SNDS data - girls/women
df_filles <- read.csv(file_filles)
df_filles$Sexe <- "Female"

# SNDS data - boys/men
df_garcons <- read.csv(file_garcons)
df_garcons$Sexe <- "Male"

# Combine both datasets into a single long-format data frame
df_snds <- bind_rows(df_filles, df_garcons)

# Format date
df_snds$Date <- as.Date(df_snds$Date)

# Set factor levels so the legend order is Girls/Women then Boys/Men
df_snds$Sexe <- factor(df_snds$Sexe, levels = c("Female", "Male"))

# Key dates and short descriptions

# Note pour Charlotte : Je te laisse le soin
# 1. d'ajouter le nom de l'agence ayant redige la reco HSH en 2016 ;
# 2. de regarder si tu es d'accord avec la traduction en anglais (dis-moi si tu souhaites effectuer des ajustements).

labels_dates <- c(
  "2007-03-09" = "Reco. for girls/women aged 11-19", # Reco filles 11-19
  "2013-01-15" = "Age-related reco. by HCSP", # Avis HCSP âge
  "2016-05-02" = "Reco. for MSM aged <26 by HCSP", # Reco HSH <26
  "2017-02-10" = "Product-related reco. by HCSP", # Reco Gardasil 9 preferentiellement
  "2019-12-19" = "Publ. of reco. for boys/men aged 11-19", # Parution reco garçons 11-19
  "2021-01-01" = "Impl. of reco. for boys/men aged 11-19", #  Application reco garçons 11-19
  "2022-08-10" = "Autho. for pharmacists to prescribe", # Vaccination par pharmaciens
  "2023-02-28" = "Ann. of school vax campaigns", # Annonce campagnes collèges
  "2023-10-02" = "Start of school vax campaigns", # Début campagnes collèges
  "2025-12-12" = "Free vax for all up to age 26", # Âge étendu à 26 ans
  "2020-03-01" = "COVID-19 lockdown", # Confinement covid
  "2011-07-15" = "First controversies", # Polémique 1
  "2013-11-15" = "MS-related controversy" # Polémique SEP
)

# Adding labels to the dataset
df_labels <- data.frame(
  Date  = as.Date(names(labels_dates)),
  label = unname(labels_dates)
)

# Parameter settings for date labels
text_angle <- 90
text_size <- 3.5

# Rows with the same horizontal and vertical adjustments
date_set_1 <- c(seq(1,7),12,13)
common_horizontal_adjustment <- 1.05
common_vertical_adjustment <- -0.4

# School-based vaccination campaigns
date_set_2 <- 8
vertical_adjustment_set_2 <- -0.6

# Start of school-based vaccination campaigns
date_set_3 <- c(9, 10)
vertical_adjustment_set_3 <- 1.4

# COVID-19 lockdown
date_set_4 <- 11
vertical_adjustment_set_4 <- 1.1

# Visualization

# Note pour Charlotte : choisis librement les couleurs des deux series
# (filles/femmes vs garcons/hommes) - ci-dessous un exemple de palette.
couleurs_sexe <- c("Female" = "#D61E33", "Male" = "#008080")

plotSNDS <- ggplot(df_snds, aes(x = Date, y = n, color = Sexe)) +

  # Dashed vertical lines
  geom_vline(xintercept = df_labels$Date, linetype = "dashed", color = "grey3", linewidth = 0.5) +

  # Most dates
  geom_text(
    data = df_labels[date_set_1,],
    aes(x = Date, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = text_angle, hjust = common_horizontal_adjustment, vjust = common_vertical_adjustment,
    size = text_size, color = "grey3"
  ) +

  # Announcement of school-based vaccination campaigns
  geom_text(data = df_labels[date_set_2,], aes(x = Date, y = Inf, label = label),
            inherit.aes = FALSE,
            angle = text_angle, hjust = common_horizontal_adjustment, vjust = vertical_adjustment_set_2,
            size = text_size, color = "grey3") +

  # Start of school-based vaccination campaigns
  geom_text(data = df_labels[date_set_3,], aes(x = Date, y = Inf, label = label),
            inherit.aes = FALSE,
            angle = text_angle, hjust = common_horizontal_adjustment, vjust = vertical_adjustment_set_3,
            size = text_size, color = "grey3") +

  # COVID-19 lockdown
  geom_text(data = df_labels[date_set_4,], aes(x = Date, y = Inf, label = label),
            inherit.aes = FALSE,
            angle = text_angle, hjust = common_horizontal_adjustment, vjust = vertical_adjustment_set_4,
            size = text_size, color = "grey3") +

  # SNDS time series, one line per sex
  geom_line(linewidth = 0.6) +

  scale_color_manual(name = "Sex", values = couleurs_sexe) +

  scale_y_continuous(name = "Number of HPV-related vaccine dispensations",
                      limits = c(0, NA),
                      labels = scales::label_comma(big.mark = ",")) +
  scale_x_date(name = 'Month/Year', date_breaks = "1 year", date_labels = "%m/%Y") +
  #coord_cartesian(clip = "off") +
  #theme_ipsum() +
  theme_bw() +

  # Other settings
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top"
    #plot.margin = margin(t = 60, r = 10, b = 10, l = 10)
  ) #+
#ggtitle("Evolution of the number of HPV vaccine doses, by sex")

plotSNDS


save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggsave(
    filename = filename,
    plot     = plot,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = dpi
  )
}

save_plot(
  plotSNDS,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/SNDS timeline by sex.png"
)
