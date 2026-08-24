library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(scales)

df <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/gardasil_vaccin\ hpv_gardasil\ et\ vaccin\ hpv.csv")

df <- df %>% 
  rename("vaccin hpv" = vaccin.hpv) %>% 
  rename( "gardasil + vaccin hpv" = "Gardasil...vaccin.hpv")

df$Date <- as.Date(paste0(df$Date, "-01"), format = "%Y-%m-%d")


df_long <- df %>%
  pivot_longer(cols = -Date, names_to = "type", values_to = "valeur")

ggplot(df_long, aes(x = Date, y = valeur, color = type, group = type)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Evolution du pourcentage de recherche google",
    x = "Date",
    y = "Nombre de vaccinations",
    color = "Equation de recherche"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
