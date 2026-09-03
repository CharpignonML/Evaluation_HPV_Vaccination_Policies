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
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "ggtrends" = "Google Trends (search intensity)",
                         "snds_f" = "SNDS (pharmacy dispensations) - female"
  ))


# Start with a usual ggplot2 call:
plot_f <- ggplot(df_long, aes(x = Date, y = n, color = Series)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations) - female" = "#800080"
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
plot_f

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
  plot_f,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation SNDS - filles GT.png"
)
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
