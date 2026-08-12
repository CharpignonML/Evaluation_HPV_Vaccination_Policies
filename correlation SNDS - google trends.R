library(dplyr)
library(tidyverse)
library(ggplot2)

## Donées SNDS

df_snds <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_hpv_dose_periode.csv")
df_snds <- df_snds %>% 
  mutate(total = dose_1 + dose_2 + dose_3)
df_snds$total <- as.numeric(df_snds$total)
df_snds <- df_snds %>%   
  mutate(total_norm = ((total - min(total)) / (max(total) - min(total)))*100)

df_snds$Date <- as.Date(paste0("01/", df_snds$periode), format = "%d/%m/%Y")
df_snds <- df_snds %>% 
  dplyr::select(Date, total_norm)


## Données google trends

df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# visuel 
df_plot <- inner_join(
  df_snds %>% select(Date, snds = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds <- ts(df_snds$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds)[, "ts_gt"]
snds <- ts.intersect(ts_gt, ts_snds)[, "ts_snds"]

# corrélation de Pearson
cor.test(gt, snds, method = "pearson")

# corrélation de spearman
cor.test(gt, snds, method = "spearman")
