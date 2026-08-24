library(causaldata)
library(modelsummary)
library(tidyr)
library(tidyverse)
library(dplyr)

# SNDS 11-14
df_1114 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_11_14.csv")
# SNDS 15-19
df_1519 <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_15_19.csv")

df_1114 <- df_1114 %>% rename("n_1114" = "n")
df_1519 <- df_1519 %>% rename("n_1519" = "n")


df <- df_1114 %>%
  inner_join(df_1519, by = "Date")

df$n_1114 <- df$n_1114 %>% 
  as.numeric()

df$n_1519 <- df$n_1519 %>% 
  as.numeric()

df$Date <- df$Date %>% 
  as.Date()

df <- df %>% 
  mutate(diff = n_1114 - n_1519)

ggplot(df, aes(x = Date, y = diff)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Evolution de la différence entre les vaccinations 11-14 et 15-19",
    x = "Date",
    y = "Différence"
  ) +
  theme_minimal()
