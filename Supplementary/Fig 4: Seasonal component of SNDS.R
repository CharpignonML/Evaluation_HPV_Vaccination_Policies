library(fpp3)
library(seasonal)
library(ggtime)
library(tseries)
library(slider) 

df_snds <- read.csv2("/Users/charlottederogis/Desktop/Stage/CSV\ populations/table_snds_total.csv")

df_snds$Date <- as.Date(df_snds$Date)

vacc <- df_snds |>
  dplyr::mutate(Date = yearmonth(Date)) |>
  as_tsibble(index = Date) |>
  fill_gaps(n = 0)

vacc |> 
  model(classical_decomposition(n, type = "additive")) |>
  components() |> 
  autoplot() +
  labs(title = "Classical Additive Decomposition")

# calculate the seasonal component

vacc_stl <- vacc |>
  model(STL(n)) |>
  components()

# Seasonal subseries plot - understand seasonal patterns
plot_mois_SNDS <- vacc_stl |> 
  ggtime::gg_subseries(season_year) +
  labs(
    x     = "Year",
    y     = "Seasonal component of HPV-related vaccine deliveries")

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
  plot_mois_SNDS,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/schémas saisonniers SNDS.png"
)
