library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)

df_f <- read.csv("/Users/charlottederogis/Desktop/cumuléfilles.csv")
df_g1 <- read.csv("/Users/charlottederogis/Desktop/cumulégarcons.csv")
df_g2 <- read.csv("/Users/charlottederogis/Desktop/cumulégarcons.csv")


df_f$temps <- interval(as.Date("2007-03-09"), df_f$Date) %/% months(1)

df_f <- df_f %>% 
  mutate(taux_couv_cum_f = taux_couverture_cumule * 100) %>% 
  select(temps, taux_couv_cum_f)

df_g1$temps <- interval(as.Date("2019-12-16"), df_g1$Date) %/% months(1)

df_g1 <- df_g1 %>% 
  mutate(taux_couv_cum_g = taux_couverture_cumule * 100) %>% 
  select(temps, taux_couv_cum_g)

df_g2$temps <- interval(as.Date("2020-12-04"), df_g2$Date) %/% months(1)
df_g2 <- df_g2 %>% 
  mutate(taux_couv_cum_g = taux_couverture_cumule * 100) %>% 
  select(temps, taux_couv_cum_g)

df <- df_f %>%
  left_join(df_g1, by = "temps") %>%
  left_join(df_g2, by = "temps")

df_long <- df %>%
  filter(temps >= 0) %>%
  pivot_longer(cols = c(taux_couv_cum_f, taux_couv_cum_g.x, taux_couv_cum_g.y),
               names_to = "serie", values_to = "taux") %>%
  filter(taux <= 15)

ggplot(df_long, aes(x = taux, y = temps, color = serie, linetype = serie)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(limits = c(0, 15), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_color_manual(
    values = c(
      "taux_couv_cum_f"   = "#882255",
      "taux_couv_cum_g.x" = "#88CCEE",
      "taux_couv_cum_g.y" = "#6699CC"
    ),
    labels = c(
      "taux_couv_cum_f"   = "Fille",
      "taux_couv_cum_g.x" = "Garçon (reco 2019)",
      "taux_couv_cum_g.y" = "Garçon (reco 2020)"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "taux_couv_cum_f"   = "solid",
      "taux_couv_cum_g.x" = "solid",
      "taux_couv_cum_g.y" = "solid"
    ),
    labels = c(
      "taux_couv_cum_f"   = "Fille",
      "taux_couv_cum_g.x" = "Garçon (reco 2019)",
      "taux_couv_cum_g.y" = "Garçon (reco 2020)"
    )
  ) +
  labs(x = "Couverture vaccinale (%)", y = "Temps depuis la reco", color = "Série", linetype = "Série") +
  theme_minimal()
