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
  pivot_longer(
    cols = -Date,
    names_to = "Search terms",
    values_to = "n"
  )
df_long <- df_long %>%
  mutate(`Search terms` = factor(trimws(`Search terms`),
                                 levels = c("gardasil", "vaccin hpv", "gardasil + vaccin hpv")))

# Start with a usual ggplot2 call:
plot_ggt <- ggplot(df_long, aes(x = Date, y = n, color = `Search terms`)) +
  
  geom_line(linewidth = 0.6) +
  
  scale_color_manual(
    name = "Search terms",
    values = c(
      "gardasil" = "red",
      "vaccin hpv" = "blue",
      "gardasil + vaccin hpv" = "green"
    )
  ) +
  scale_y_continuous(
    name = "Google search intensity",
    limits = c(NA, NA),
    labels = scales::label_comma(big.mark = ",")) +
  scale_x_date(name = 'Month/Year', date_breaks = "1 year", date_labels = "%m/%Y") +
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
  )
plot_ggt

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
  plot_ggt,
  "/Users/charlottederogis/Desktop/Stage/VF\ analyses/Analyses VF code et figures/google trends comparaison termes.png"
)
