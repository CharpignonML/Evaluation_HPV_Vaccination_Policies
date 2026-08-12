library(dplyr)
library(tidyverse)
library(ggplot2)

## Donées SNDS - filles

df_f <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_filles.csv")

df_f <- df_f %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_f$Date <- as.Date(df_f$Date)
df_f <- df_f %>% 
  dplyr::select(Date, total_norm)

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")


# visuel 
df_plot <- inner_join(
  df_f %>% select(Date, snds_f = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_f, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - filles et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_f <- ts(df_f$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_f)[, "ts_gt"]
snds_f <- ts.intersect(ts_gt, ts_snds_f)[, "ts_snds_f"]

# corrélation de Pearson
cor.test(gt, snds_f, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_f, method = "spearman")



## Donées SNDS - garcons

df_g <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_garcons.csv")

df_g <- df_g %>%   
  mutate(total_norm = ((n - min(n)) / (max(n) - min(n)))*100)
df_g$Date <- as.Date(df_g$Date)
df_g <- df_g %>% 
  dplyr::select(Date, total_norm)

## Données google trends
df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")
df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# visuel 
df_plot <- inner_join(
  df_g %>% select(Date, snds_g = total_norm),
  df_ggtrends %>% select(Date, ggtrends = n),
  by = "Date"
)
df_long <- df_plot %>% 
  tidyr::pivot_longer(cols = c(snds_g, ggtrends), 
                      names_to = "serie", 
                      values_to = "valeur")
ggplot(df_long, aes(x = Date, y = valeur, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Comparaison SNDS - garcons et Google Trends",
       x = "Date", y = "Valeur",
       color = "Série") +
  theme_minimal()

# transformation en time series
ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_g <- ts(df_g$total_norm, start = c(2007, 07), frequency = 12)

# même dates
gt <- ts.intersect(ts_gt, ts_snds_g)[, "ts_gt"]
snds_g <- ts.intersect(ts_gt, ts_snds_g)[, "ts_snds_g"]

# corrélation de Pearson
cor.test(gt, snds_g, method = "pearson")

# corrélation de spearman
cor.test(gt, snds_g, method = "spearman")

## SNDS Garcons a partir de la reco - 2019

df_ggtrends_2019 <- df_ggtrends %>% 
  filter(Date >= as.Date("2019-12-01"))

df_g_2019 <- df_g %>% 
  filter(Date >= as.Date("2019-12-01"))

# transformation en time series
ts_gt_2019 <- ts(df_ggtrends_2019$n, start = c(2019, 12), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_g_2019 <- ts(df_g_2019$total_norm, start = c(2019, 12), frequency = 12)

# même dates
gt_2019 <- ts.intersect(ts_gt_2019, ts_snds_g_2019)[, "ts_gt_2019"]
snds_g_2019 <- ts.intersect(ts_gt_2019, ts_snds_g_2019)[, "ts_snds_g_2019"]

# corrélation de Pearson
cor.test(gt_2019, snds_g_2019, method = "pearson")

# corrélation de spearman
cor.test(gt_2019, snds_g_2019, method = "spearman")

## SNDS Garcons a partir de la reco - 2020

df_ggtrends_2020 <- df_ggtrends %>% 
  filter(Date >= as.Date("2020-12-01"))

df_g_2020 <- df_g %>% 
  filter(Date >= as.Date("2020-12-01"))

# transformation en time series
ts_gt_2020 <- ts(df_ggtrends_2020$n, start = c(2020, 12), frequency = 12)  # ou frequency = 1 si annuel
ts_snds_g_2020 <- ts(df_g_2020$total_norm, start = c(2020, 12), frequency = 12)

# même dates
gt_2020 <- ts.intersect(ts_gt_2020, ts_snds_g_2020)[, "ts_gt_2020"]
snds_g_2020 <- ts.intersect(ts_gt_2020, ts_snds_g_2020)[, "ts_snds_g_2020"]

# corrélation de Pearson
cor.test(gt_2020, snds_g_2020, method = "pearson")

# corrélation de spearman
cor.test(gt_2020, snds_g_2020, method = "spearman")
