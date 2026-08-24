library(fpp3)
library(dplyr)
library(seasonal)
library(ggtime)
library(tsibble)
library(readr)

# SNDS

df <- read.csv2("/Users/charlottederogis/Desktop/Stage/données\ SNDS/vente\ par\ mois.csv")

df$Date <- as.Date(paste0("01-", df$Date), format = "%d-%m-%y")

tsibble <- as_tsibble(df)

tsibble <- tsibble |> 
  filter(Date >= as.Date("2007-07-01"))

tsibble <- tsibble |> 
  mutate(Date = yearmonth(Date)) |>
  update_tsibble(index = Date)

ventes_add <- tsibble |> 
  model(classical_decomposition(n, type = "additive")) |>
  components()

tsibble |> 
  model(classical_decomposition(n, type = "additive")) |>
  components() |> 
  autoplot() +
  labs(title = "Classical Additive Decomposition")

ventes_add |> 
  as_tibble() |> 
  select(Date, trend) |> 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_ventes.csv")

range(tsibble$n)
# 182639

ventes_add %>% 
  as_tibble() %>% 
  mutate(seasonal_sur_range = seasonal / 182639) %>% 
  select(Date, seasonal_sur_range) %>% 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_ventes.csv")

ventes_add %>% 
  as_tibble() %>% 
  mutate(random_sur_range = random / 182639) %>% 
  select(Date, random_sur_range) %>% 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_ventes.csv")

# google trends

df <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")

df$Date <- as.Date(paste0(df$Date, "-01"), format = "%Y-%m-%d")

df$Date <- as.Date(paste0("-01", df$Date), format = "%y-%m-%d")

tsibble <- as_tsibble(df)

tsibble <- tsibble |> 
  filter(Date >= as.Date("2007-07-01"))

tsibble <- tsibble |> 
  mutate(Date = yearmonth(Date)) |>
  update_tsibble(index = Date)

recherches_add <- tsibble |> 
  model(classical_decomposition(n, type = "additive")) |>
  components()

tsibble |> 
  model(classical_decomposition(n, type = "additive")) |>
  components() |> 
  autoplot() +
  labs(title = "Classical Additive Decomposition")

recherches_add |> 
  as_tibble() |> 
  select(Date, trend) |> 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_recherches.csv")

range(tsibble$n)
# 94

recherches_add %>% 
  as_tibble() %>% 
  mutate(seasonal_sur_range = seasonal / 94) %>% 
  select(Date, seasonal_sur_range) %>% 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_recherches.csv")

recherches_add %>% 
  as_tibble() %>% 
  mutate(random_sur_range = random / 94) %>% 
  select(Date, random_sur_range) %>% 
  write_csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_recherches.csv")
