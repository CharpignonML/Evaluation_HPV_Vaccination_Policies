library(causaldata)
library(modelsummary)
library(tidyr)
library(tidyverse)
library(dplyr)

# SNDS 11-14
df_1114 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_11_14.csv")
# SNDS 20-26
df_2026 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_20_26.csv")

df_1114 <- df_1114 %>% rename("n_1114" = "n")
df_2026 <- df_2026 %>% rename("n_2026" = "n")


df <- df_1114 %>%
  inner_join(df_2026, by = "Date")

df$n_1114 <- df$n_1114 %>% 
  as.numeric()

df$n_2026 <- df$n_2026 %>% 
  as.numeric()

df$Date <- df$Date %>% 
  as.Date()

df <- df %>% 
  mutate(diff = n_1114 - n_2026)

ggplot(df, aes(x = Date, y = diff)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Evolution de la différence entre les vaccinations 11-14 et 20-26",
    x = "Date",
    y = "Différence"
  ) +
  theme_minimal()
