library(dplyr)
library(seasonal)
library(fpp3)
library(ggtime)

cv <- read.csv2("/Users/charlottederogis/Desktop/ggtrends_calendriervacc.csv")

cv <- cv %>%
  mutate(Date = yearmonth(Date))

cv <- cv %>% as_tsibble(index = Date)

df_stl <- cv |>
  model(STL(n)) |>
  components()

df_stl |> 
  ggtime::gg_subseries(season_year) +
  labs(title = "Seasonal Patterns by months",
       subtitle = "Shows consistency of seasonal effects")
