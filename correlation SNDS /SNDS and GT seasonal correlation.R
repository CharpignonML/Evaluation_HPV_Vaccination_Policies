library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)


## Donées SNDS

df_snds_season <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_ventes.csv")

df_snds_season$Date <- as.Date(paste0(df_snds_season$Date, " 01"), format = "%Y %b %d")

df_snds_season <- df_snds_season %>% 
  rename(seasonal_snds_nor = seasonal_sur_range)

## Données google trends

df_ggtrends_season <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/seasonal_recherches.csv")

df_ggtrends_season$Date <- as.Date(paste0(df_ggtrends_season$Date, " 01"), format = "%Y %b %d")

df_ggtrends_season <- df_ggtrends_season %>% 
  rename(seasonal_ggtrends_nor = seasonal_sur_range)

range(df_ggtrends_season$seasonal_ggtrends_nor, na.rm = TRUE)
# min = -0,035 ; max = 0,042
range(df_snds_season$seasonal_snds_nor, na.rm = TRUE)
# min = -0,053 ; max = 0,101

df_season_merged <- left_join(df_ggtrends_season, df_snds_season, by = "Date")

ggplot(df_season_merged, aes(x=Date)) +
  
  geom_line( aes(y=`seasonal_ggtrends_nor`), color = "#882E72") + 
  geom_line( aes(y=`seasonal_snds_nor`), color = "#F1932D") + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Seasonality of researches linked with HPV intensity",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name = "seasonality of HPV related vaccine dispensation")
  ) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
  theme(
    axis.title.y = element_text(color = "#882E72", size=13),
    axis.title.y.right = element_text(color = "#F1932D", size=13)
  ) +
  
  ggtitle("trends of HPV related google searches and HPV vaccines dispensations")


# calcul de la correlation

df_season_merged <- na.omit(df_season_merged)
cor.test(df_season_merged$seasonal_ggtrends_nor, df_season_merged$seasonal_snds_nor, method = "pearson")
cor.test(df_season_merged$seasonal_ggtrends_nor, df_season_merged$seasonal_snds_nor, method = "spearman")

# pour la visibilité:

ggplot(df_season_merged, aes(x=Date)) +
  
  geom_line( aes(y=`seasonal_ggtrends_nor`), color = "#882E72") + 
  geom_line( aes(y=`seasonal_snds_nor`), color = "#F1932D") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "saisonalité des recherches google",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name = "saisonalité du nombre de ventes")
  ) + 
  scale_x_date(
    date_breaks = "2 months", 
    date_labels = "%m/%Y",
    limits = as.Date(c("2012-01-01", "2014-01-01"))
  ) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
  theme(
    axis.title.y = element_text(color = "#882E72", size=13),
    axis.title.y.right = element_text(color = "#F1932D", size=13)
  ) +
  
  ggtitle("saisonalité des Recherches googles en lien avec le vaccin HPV et nombre de ventes de vaccin HPV au cours du temps")
