library(dplyr)
library(tidyverse)
library(ggplot2)

## Donées SNDS - 11-14

df_1114 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_11_14.csv")

df_1114 <- df_1114 %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_1114$Date <- as.Date(df_1114$Date)
df_1114 <- df_1114 %>% 
  dplyr::select(Date, total_norm)

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")


# visuel 
df_plot <- inner_join(
  df_1114 %>% select(Date, snds_1114 = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_1114, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - 11-14 et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_1114 <- ts(df_1114$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_1114)[, "ts_gt"]
snds_1114 <- ts.intersect(ts_gt, ts_snds_1114)[, "ts_snds_1114"]

# corrélation de Pearson
cor.test(gt, snds_1114, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_1114, method = "spearman")



## Donées SNDS - 15-19

df_1519 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_15_19.csv")

df_1519 <- df_1519 %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_1519$Date <- as.Date(df_1519$Date)
df_1519 <- df_1519 %>% 
  dplyr::select(Date, total_norm)

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# visuel 
df_plot <- inner_join(
  df_1519 %>% select(Date, snds_1519 = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_1519, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - 15-19 et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_1519 <- ts(df_1519$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_1519)[, "ts_gt"]
snds_1519 <- ts.intersect(ts_gt, ts_snds_1519)[, "ts_snds_1519"]

# corrélation de Pearson
cor.test(gt, snds_1519, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_1519, method = "spearman")


## Donées SNDS - plus de 26

df_26plus <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_26plus.csv")

df_26plus <- df_26plus %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_26plus$Date <- as.Date(df_26plus$Date)
df_26plus <- df_26plus %>% 
  dplyr::select(Date, total_norm)

# pour regler le problème des données maquantes
all_months <- data.frame(Date = seq(min(df_26plus$Date), max(df_26plus$Date), by = "month"))

df_26plus_full <- all_months %>% 
  left_join(df_26plus, by = "Date")

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# visuel 
df_plot <- inner_join(
  df_26plus %>% select(Date, snds_26plus = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_26plus, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - 26 plus et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_26plus <- ts(df_26plus_full$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_26plus)[, "ts_gt"]
snds_26plus <- ts.intersect(ts_gt, ts_snds_26plus)[, "ts_snds_26plus"]

# corrélation de Pearson
cor.test(gt, snds_26plus, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_26plus, method = "spearman")

## Donées SNDS - 20-26

df_2026 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_20_26.csv")

df_2026 <- df_2026 %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_2026$Date <- as.Date(df_2026$Date)
df_2026 <- df_2026 %>% 
  dplyr::select(Date, total_norm)

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# visuel 
df_plot <- inner_join(
  df_2026 %>% select(Date, snds_2026 = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_2026, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - 20-26 et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_2026 <- ts(df_2026$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_2026)[, "ts_gt"]
snds_2026 <- ts.intersect(ts_gt, ts_snds_2026)[, "ts_snds_2026"]

# corrélation de Pearson
cor.test(gt, snds_2026, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_2026, method = "spearman")
