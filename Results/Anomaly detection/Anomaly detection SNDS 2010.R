library("tidyverse")
library("tibbletime")
library("anomalize")
library("lubridate")
library("quantmod") #getsymbols
library("tbl2xts")
library(tibble)
library(dplyr)

df_snds <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_hpv_dose_periode.csv")

df_snds <- df_snds %>% 
  mutate(total = dose_1 + dose_2 + dose_3)

df_snds$Date <- as.Date(paste0("01/", df_snds$periode), format = "%d/%m/%Y")

df_snds <- df_snds %>% 
  select(Date, total)

df_2010 <- df_snds %>%
  filter(Date >= as.Date("2010-01-01"))

ggplot(data = df_2010, aes(x = Date, y = total)) +
  geom_line(color = "indianred3", size = 1) +
  geom_smooth(method = "loess") +
  scale_x_date(date_labels = "%b/%Y") +
  labs(
    title = "Nombre de ventes de vaccins HPV",
    subtitle = "2010 - 2026",
    x = "",
    y = "Nombre de ventes"
  ) +
  theme_minimal()

snds_tbl <- df_2010 %>%
  as_tibble() %>%
  mutate(Date = as.Date(Date))

snds_tbl %>%
  time_decompose(total,
                 method = "stl",
                 frequency = "auto",
                 trend = "auto") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Anomalies vente de vaccin",
       subtitle = "Method STL Decomposition")

anomalies <- snds_tbl %>%
  time_decompose(total,
                 method = "stl",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == "Yes")

# Voir les dates et valeurs
anomalies %>% select(Date, observed, remainder, anomaly)


acf(snds_tbl$total)

# s'affranchir de la tendance

snds_tbl <- snds_tbl %>%
  mutate(total.diff = c(0, diff(total)))

acf(snds_tbl$total.diff)  

snds_tbl %>%
  time_decompose(total.diff,
                 method = "stl",
                 frequency = "auto",
                 trend = "auto") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Anomalies ventes vaccin - Différence",
       subtitle = "Method STL Decomposition")

# Voir les dates et valeurs
anomalies <- snds_tbl %>%
  time_decompose(total.diff,
                 method = "stl",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == "Yes")
anomalies %>% select(Date, observed, remainder, anomaly)

# s'affranchir de l'Heteroscedasticity

# Difference for logarithms for Tesla shares prices
snds_tbl <- snds_tbl %>%
  mutate(total.Diff.Log = c(0,diff(log(total))))

#Plot
snds_tbl %>%
  time_decompose(total.Diff.Log,
                 method = "stl",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Anomalies ventes de vaccin Difference of the log",
       subtitle = "Method: STL Decomposition")

anomalies <- snds_tbl %>%
  time_decompose(total.Diff.Log,
                 method = "stl",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == "Yes")
anomalies %>% select(Date, observed, remainder, anomaly)

# methode twitter 

snds_tbl %>%
  time_decompose(total.Diff.Log,
                 method = "twitter",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Anomalies ventes de vaccin for the Difference of the log",
       subtitle = "Method: Twitter Decomposition")

anomalies <- snds_tbl %>%
  time_decompose(total.Diff.Log,
                 method = "twitter",
                 frequency = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == "Yes")
anomalies %>% select(Date, observed, remainder, anomaly)

# methode IQR

ventes_iqr_outliers <- iqr(snds_tbl$total.Diff.Log,
                           alpha = 0.05,
                           max_anoms = 0.2,
                           verbose = TRUE)$outlier_report

# Ploting function for anomaly observations
# Basis taken from:
# https://business-science.github.io/anomalize/articles/anomalize_methods.html
ggsetup <- function(data) {
  #Scale y axis
  y_axis <- max(abs(data$value)) + mean(abs(data$value))
  data %>%
    ggplot(aes(rank, value, color = outlier)) +
    geom_point() +
    geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
    geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
    geom_text(aes(label = index), vjust = -1.25) +
    theme_bw() +
    scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
    ylim(- y_axis, y_axis) +
    theme(legend.position = "bottom")
}

ventes_iqr_outliers %>%
  ggsetup() +
  ggtitle("IQR: Top outliers sorted by rank")
