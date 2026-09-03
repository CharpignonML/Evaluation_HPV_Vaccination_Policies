library(fpp3)
library(seasonal)
library(ggtime)
library(tseries)
library(slider) 

## GT

df_gt <- read.csv2("/Users/charlottederogis/Desktop/Stage/CSV\ populations/GT/ggt total dates snds.csv")

df_gt$Date <- as.Date(paste0(df_gt$Date, "-01"))

vacc <- df_gt |>
  dplyr::mutate(Date = yearmonth(Date)) |>
  as_tsibble(index = Date) |>
  fill_gaps(n = 0)

plot_1<- vacc |> 
  model(classical_decomposition(n, type = "additive")) |>
  components() |> 
  autoplot() +
  labs(title = "Classical Additive Decomposition of GT data")

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
  plot_1,
  "/Users/charlottederogis/Desktop/STL GT.png"
)

## SNDS

df_snds <- read.csv2("/Users/charlottederogis/Desktop/Stage/CSV\ populations/table_snds_total.csv")

df_snds$Date <- as.Date(df_snds$Date)

vacc <- df_snds |>
  dplyr::mutate(Date = yearmonth(Date)) |>
  as_tsibble(index = Date) |>
  fill_gaps(n = 0)

plot_2 <- vacc |> 
  model(classical_decomposition(n, type = "additive")) |>
  components() |> 
  autoplot() +
  labs(title = "Classical Additive Decomposition of SNDS data")

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
  plot_2,
  "/Users/charlottederogis/Desktop/STL SNDS.png"
)
