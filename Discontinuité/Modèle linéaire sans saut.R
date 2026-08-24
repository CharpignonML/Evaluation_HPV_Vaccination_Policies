library(causaldata)
library(modelsummary)
library(tidyr)
library(tidyverse)

# Données: 

# SNDS garcons
df <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_garcons.csv")
# SNDS filles
df <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_snds_filles.csv")

# SNDS 11-14 ans 
df <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_11_14.csv")
# SNDS 15-19 ans 
df <- read.csv("/Users/charlottederogis/Desktop/Stage/Données\ SNDS\ bis/table_snds_15_19.csv")

# SNDS prescription par pharmaciens 
df <- read.csv("/Users/charlottederogis/Desktop/Stage/SNDS\ prescripteur\ corrigé/table_snds_50.csv")

# SNDS
df <- read.csv2("/Users/charlottederogis/Desktop/Stage/données\ SNDS/vente\ par\ mois.csv")
df$Date <- as.Date(paste0("1-", df$Date), format = "%d-%m-%y")

# Compléter avec la date de la discontinuité
CUTOFF    <- as.Date("2017-09-13")

# Intervalle: 365 * 2, 3 ou 4 en fonction de l'intervalle considéré
BANDWIDTH <- 365 * 3

df <- df %>%
  mutate(
    Date    = as.Date(Date),
    t       = as.integer(Date - as.Date(CUTOFF)),
    treated = as.integer(Date >= CUTOFF)
  ) %>% 
  filter(abs(t) <= BANDWIDTH)  

# Modele "kink": pas de saut de niveau au cutoff, seulement un changement de pente.

m_kink <- lm(n ~ t + t:treated, data = df)

# Valeurs resultant du modele
df$fitted_m_kink <- m_kink$fitted.values

# Transform the dataset into a long format
df_long <- df %>% 
  pivot_longer(cols = c(n, fitted_m_kink), 
               names_to = "model", 
               values_to = "fitted_value")
df_long$model_f <- as.factor(df_long$model)

# Summary of models
msummary(list('Coefficients' = m_kink),
         stars = c('*' = .1, '**' = .05, '***' = .01), 
         vcov = 'robust',
         statistic = c("conf.int", "p.value"),
         gof_map = c("r.squared", "adj.r.squared"))


# Visualisation pour SNDS
p <- ggplot(df_long, aes(x = t/365.25, y = fitted_value, group = model_f)) +
  geom_line(aes(color = model_f)) +
  geom_point(aes(color = model_f)) +
  xlab('Number of years before/after time zero') +
  ylab('Number of HPV vaccine dispensation') +
  labs(color = "Model") +
  scale_color_discrete(labels = c("fitted_m_kink" = "linear model with no jump",
                                  "n" = "Observed data")) +
  geom_vline(xintercept = 0, lty = 2) +
  ggtitle('Time zero: AMM gardasil9 (13 septembre 2017)\nTime period: 3 years before/after time zero\nData: SNDS, monthly') +
  theme_bw()
p
