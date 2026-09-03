# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)

# Load data
# Specify your own paths
df_f <- read.csv("/Users/charlottederogis/Desktop/cumuléfilles.csv")
df_g <- read.csv("/Users/charlottederogis/Desktop/cumulégarcons.csv")

# Format dates and compute months elapsed since each recommendation date
# Filles : reco du 09/03/2007
df_f$Date <- as.Date(df_f$Date)  # ajuster le format si besoin, ex: format = "%d/%m/%Y"
df_f$temps <- interval(as.Date("2007-03-09"), df_f$Date) %/% months(1)
df_f <- df_f %>%
  mutate(taux_couv_cum_f = taux_couverture_cumule * 100) %>%
  select(temps, taux_couv_cum_f)

# Garçons : reco du 01/01/2021
df_g$Date <- as.Date(df_g$Date)  # ajuster le format si besoin
df_g$temps <- interval(as.Date("2021-01-01"), df_g$Date) %/% months(1)
df_g <- df_g %>%
  mutate(taux_couv_cum_g = taux_couverture_cumule * 100) %>%
  select(temps, taux_couv_cum_g)

# Join on months-since-reco so both series share a common time axis
df <- df_f %>%
  left_join(df_g, by = "temps")

# Long format for plotting the two series together
df_long <- df %>%
  filter(temps >= 0) %>%
  pivot_longer(cols = c(taux_couv_cum_f, taux_couv_cum_g),
               names_to = "serie", values_to = "taux") %>%
  filter(taux <= 15)

# Visualization (axes inversés : temps en x, taux en y)
ggplot(df_long, aes(x = temps, y = taux, color = serie, linetype = serie)) +
  
  # Vaccination coverage curves
  geom_line(linewidth = 0.8) +
  scale_x_continuous(name = "Number of months since the publication of the HCSP recommendation",
                     limits = c(0, 25)) +
  scale_y_continuous(name = "Vaccination coverage (%)",
                     limits = c(0, 15),
                     labels = function(x) paste0(x)) +
  
  scale_color_manual(
    name = "Sex",
    values = c(
      "taux_couv_cum_f" = "#D61E33",
      "taux_couv_cum_g" = "#008080"
    ),
    labels = c(
      "taux_couv_cum_f" = "Female",
      "taux_couv_cum_g" = "Male"
    )
  ) +
  scale_linetype_manual(
    name = "Sex",
    values = c(
      "taux_couv_cum_f" = "solid",
      "taux_couv_cum_g" = "solid"
    ),
    labels = c(
      "taux_couv_cum_f" = "Female",
      "taux_couv_cum_g" = "Male"
    )
  ) +
  
  theme_bw() +
  
  # Other settings, sur le modele des instructions
  theme(
    axis.text.x  = element_text(angle = 0,  vjust = 0.5, size = 12, face = "bold"),
    axis.text.y  = element_text(angle = 0,  vjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )
