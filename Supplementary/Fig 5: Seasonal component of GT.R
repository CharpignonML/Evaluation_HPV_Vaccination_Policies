library(ggtime)
library(fpp3)
library(seasonal)
library(tseries)
library(slider) 


df_gt <- read.csv2("/Users/charlottederogis/Desktop/Stage/CSV\ populations/GT/ggt\ total\ dates\ snds.csv")

df_gt$Date <- as.Date(paste0(df_gt$Date, "-01"))

vacc <- df_gt |>
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
plot_mois_gt <- vacc_stl |> 
  mutate(.model = "") |> # pour enlever stl(n) dans la box grise a droite
  ggtime::gg_subseries(season_year) +
  labs(
    x     = "Year",
    y     = "Seasonal component of HPV-related Google search intensity")

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
  plot_mois_gt,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/schémas saisonniers GT.png"
)
