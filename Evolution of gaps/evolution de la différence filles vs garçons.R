library(causaldata)
library(modelsummary)
library(tidyr)
library(tidyverse)
library(dplyr)

# SNDS garcons
df_g <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_garcons.csv")
# SNDS filles
df_f <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_filles.csv")

df_g <- df_g %>% rename("n garcons" = "n")
df_f <- df_f %>% rename("n filles" = "n")


df <- df_g %>%
  inner_join(df_f, by = "Date")

df$`n garcons` <- df$`n garcons` %>% 
  as.numeric()

df$`n filles` <- df$`n filles` %>% 
  as.numeric()

df$Date <- df$Date %>% 
  as.Date()

df <- df %>% 
  mutate(diff = `n filles` - `n garcons`)

ggplot(df, aes(x = Date, y = diff)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Evolution de la différence entre les vaccinations filles et garcons",
    x = "Date",
    y = "Différence"
  ) +
  theme_minimal()  
