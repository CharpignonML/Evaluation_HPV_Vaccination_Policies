library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)


df_snds_trend <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_ventes.csv")

df_snds_trend$Date <- as.Date(paste0(df_snds_trend$Date, " 01"), format = "%Y %b %d")

range(df_snds_trend$trend, na.rm = TRUE)
#max = 137936.54

## Données google trends
df_ggtrends_trend <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_recherches.csv")
df_ggtrends_trend$Date <- as.Date(paste0(df_ggtrends_trend$Date, " 01"), format = "%Y %b %d")
range(df_ggtrends_trend$trend, na.rm = TRUE)
# max = 58,4

df_trends_merged <- left_join(df_ggtrends_trend, df_snds_trend, by = "Date")
df_trends_merged$trend_snds_nor <- ((df_trends_merged$trend.y) / 137936.54)
df_trends_merged$trend_google_nor <- ((df_trends_merged$trend.x) / 58.4)
df_trends_merged <- df_trends_merged |> dplyr::select(Date, trend_snds_nor, trend_google_nor)

df_trends_long <- df_trends_merged %>%
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "trend_google_nor" = "Google Trends (search intensity)",
                         "trend_snds_nor" = "SNDS (pharmacy dispensations)"
  ))


# Start with a usual ggplot2 call:
plot_trend <- ggplot(df_trends_long, aes(x = Date, y = n, color = Series)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations)" = "#800080"
    )
  ) +
  scale_y_continuous(
    name = "Trends (normalized)",
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
plot_trend

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
  plot_trend,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation trend SNDS GT.png"
)

# calcul de la correlation

df_trends_merged <- na.omit(df_trends_merged)
cor.test(df_trends_merged$trend_google, df_trends_merged$trend_snds, method = "pearson")
cor.test(df_trends_merged$trend_google, df_trends_merged$trend_snds, method = "spearman")
