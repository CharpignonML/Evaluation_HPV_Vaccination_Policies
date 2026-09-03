library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)


## Donées SNDS

df_snds_season <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_ventes.csv")

df_snds_season$Date <- as.Date(paste0(df_snds_season$Date, " 01"), format = "%Y %b %d")

df_snds_season <- df_snds_season %>% 
  rename(seasonal_snds_nor = seasonal_sur_range)

## Données google trends

df_ggtrends_season <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_recherches.csv")

df_ggtrends_season$Date <- as.Date(paste0(df_ggtrends_season$Date, " 01"), format = "%Y %b %d")

df_ggtrends_season <- df_ggtrends_season %>% 
  rename(seasonal_ggtrends_nor = seasonal_sur_range)

range(df_ggtrends_season$seasonal_ggtrends_nor, na.rm = TRUE)
# min = -0,035 ; max = 0,042
range(df_snds_season$seasonal_snds_nor, na.rm = TRUE)
# min = -0,053 ; max = 0,101

df_season_merged <- left_join(df_ggtrends_season, df_snds_season, by = "Date")

df_season_long <- df_season_merged %>%
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "seasonal_ggtrends_nor" = "Google Trends (search intensity)",
                         "seasonal_snds_nor" = "SNDS (pharmacy dispensations)"
  ))


# Start with a usual ggplot2 call:
plot_season <- ggplot(df_season_long, aes(x = Date, y = n, color = Series)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations)" = "#800080"
    )
  ) +
  scale_y_continuous(
    name = "Seasonality (normalized)",
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
plot_season

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
  plot_season,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation saisonalité SNDS GT.png"
)

# calcul corrélation

df_season_merged <- na.omit(df_season_merged)
cor.test(df_season_merged$seasonal_ggtrends_nor, df_season_merged$seasonal_snds_nor, method = "pearson")
cor.test(df_season_merged$seasonal_ggtrends_nor, df_season_merged$seasonal_snds_nor, method = "spearman")
