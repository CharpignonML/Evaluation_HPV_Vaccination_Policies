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
df_plot_long <- df_plot %>%
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "ggtrends" = "Google Trends (search intensity)",
                         "snds_1114" = "SNDS (pharmacy dispensations) for 11-14"
  ))


# Start with a usual ggplot2 call:
plot_1114 <- ggplot(df_plot_long, aes(x = Date, y = n, color = Series)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations) for 11-14" = "#800080"
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
plot_1114

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
  plot_1114,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation SNDS 11-14 GT.png"
)

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
