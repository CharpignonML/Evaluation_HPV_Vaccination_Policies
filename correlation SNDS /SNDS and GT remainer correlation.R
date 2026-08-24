library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)

## Donées SNDS

df_snds_reste <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_ventes.csv")

df_snds_reste$Date <- as.Date(paste0(df_snds_reste$Date, " 01"), format = "%Y %b %d")

df_snds_reste <- df_snds_reste %>% 
  rename(reste_snds_nor = random_sur_range)

## Données google trends

df_ggtrends_reste <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/random_recherches.csv")

df_ggtrends_reste$Date <- as.Date(paste0(df_ggtrends_reste$Date, " 01"), format = "%Y %b %d")

df_ggtrends_reste <- df_ggtrends_reste %>% 
  rename(reste_ggtrends_nor = random_sur_range)

range(df_ggtrends_reste$reste_ggtrends_nor, na.rm = TRUE)
# min = -0,181 ; max = 0,522
range(df_snds_reste$reste_snds_nor, na.rm = TRUE)
# min = -0,147 ; max = 0,215

df_reste_merged <- left_join(df_ggtrends_reste, df_snds_reste, by = "Date")

ggplot(df_reste_merged, aes(x=Date)) +
  
  geom_line( aes(y=`reste_ggtrends_nor`), color = "#882E72") + 
  geom_line( aes(y=`reste_snds_nor`), color = "#F1932D") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "reste des recherches google",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name = "reste du nombre de ventes")
  ) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
  theme(
    axis.title.y = element_text(color = "#882E72", size=13),
    axis.title.y.right = element_text(color = "#F1932D", size=13)
  ) +
  
  ggtitle("reste des Recherches googles en lien avec le vaccin HPV et nombre de ventes de vaccin HPV au cours du temps")


# calcul de la correlation

df_reste_merged <- na.omit(df_reste_merged)
cor.test(df_reste_merged$reste_ggtrends_nor, df_reste_merged$reste_snds_nor, method = "pearson")
cor.test(df_reste_merged$reste_ggtrends_nor, df_reste_merged$reste_snds_nor, method = "spearman")
