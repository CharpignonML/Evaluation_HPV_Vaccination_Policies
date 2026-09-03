library(dplyr)
library(tidyverse)
library(ggplot2)

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
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "ggtrends" = "Google Trends (search intensity)",
                         "snds_g" = "SNDS (pharmacy dispensations) - males"
  ))


# Start with a usual ggplot2 call:
plot_g <- ggplot(df_long, aes(x = Date, y = n, color = Series)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations) - males" = "#800080"
    )
  ) +
  scale_y_continuous(
    name = "Intensity (percentage)",
    limits = c(NA, NA),
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
  )
plot_g

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
  plot_g,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation SNDS - boys GT.png"
)

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

# t
library(cocor)

# On doit avoir les 3 séries alignées sur les mêmes dates
ts_all <- ts.intersect(ts_gt, ts_snds_f, ts_snds_g)
gt_c    <- ts_all[, "ts_gt"]
snds_f_c <- ts_all[, "ts_snds_f"]
snds_g_c <- ts_all[, "ts_snds_g"]

# Les trois corrélations nécessaires
r_gt_f <- cor(gt_c, snds_f_c, method = "pearson")   # corrélation GT - filles
r_gt_g <- cor(gt_c, snds_g_c, method = "pearson")   # corrélation GT - garçons
r_f_g  <- cor(snds_f_c, snds_g_c, method = "pearson") # corrélation filles - garçons

n <- length(gt_c)  # taille de l'échantillon commun

# Test de comparaison de corrélations dépendantes (overlapping)
cocor.dep.groups.overlap(
  r.jk = r_gt_f,   # corrélation entre GT (j) et filles (k)
  r.jh = r_gt_g,   # corrélation entre GT (j) et garçons (h)
  r.kh = r_f_g,    # corrélation entre filles (k) et garçons (h)
  n = n,
  alternative = "two.sided",
  test = "steiger1980",  # ou "meng1992"
  alpha = 0.05
)



## SNDS Garcons a partir de la reco - 2021

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
