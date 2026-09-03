library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)

## Donées SNDS

df_snds_reste <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_ventes.csv")

df_snds_reste$Date <- as.Date(paste0(df_snds_reste$Date, " 01"), format = "%Y %b %d")

df_snds_reste <- df_snds_reste %>% 
  rename(reste_snds_nor = random_sur_range)

## Données google trends

df_ggtrends_reste <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_recherches.csv")

df_ggtrends_reste$Date <- as.Date(paste0(df_ggtrends_reste$Date, " 01"), format = "%Y %b %d")

df_ggtrends_reste <- df_ggtrends_reste %>% 
  rename(reste_ggtrends_nor = random_sur_range)

range(df_ggtrends_reste$reste_ggtrends_nor, na.rm = TRUE)
# min = -0,181 ; max = 0,522
range(df_snds_reste$reste_snds_nor, na.rm = TRUE)
# min = -0,147 ; max = 0,215

df_reste_merged <- left_join(df_ggtrends_reste, df_snds_reste, by = "Date")

df_reste_long <- df_reste_merged %>%
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "n"
  ) %>%
  mutate(Series = recode(Series,
                         "reste_ggtrends_nor" = "Google Trends (search intensity)",
                         "reste_snds_nor" = "SNDS (pharmacy dispensations)"
  ))

# Start with a usual ggplot2 call:
plot_reste <- ggplot(df_reste_long, aes(x = Date, y = n, color = Series)) +

  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Series",
    values = c(
      "Google Trends (search intensity)" = "#C75915",
      "SNDS (pharmacy dispensations)" = "#800080"
    )
  ) +
  scale_y_continuous(
        name = "Remainder (normalized)",
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
  plot_reste,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/correlation reste SNDS et GT.png"
)
# calcul de la correlation

df_reste_merged <- na.omit(df_reste_merged)
cor.test(df_reste_merged$reste_ggtrends_nor, df_reste_merged$reste_snds_nor, method = "pearson")
cor.test(df_reste_merged$reste_ggtrends_nor, df_reste_merged$reste_snds_nor, method = "spearman")
