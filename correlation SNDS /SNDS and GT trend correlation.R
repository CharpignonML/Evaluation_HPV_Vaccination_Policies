library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(tsibble)

## Donées SNDS

df_snds_trend <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_ventes.csv")

df_snds_trend$Date <- as.Date(paste0(df_snds_trend$Date, " 01"), format = "%Y %b %d")

range(df_snds_trend$trend, na.rm = TRUE)

#max = 137936.54

## Données google trends

df_ggtrends_trend <- read.csv("/Users/charlottederogis/Desktop/Stage/R/Correlation/trend_recherches.csv")

df_ggtrends_trend$Date <- as.Date(paste0(df_ggtrends_trend$Date, " 01"), format = "%Y %b %d")

range(df_ggtrends_trend$trend, na.rm = TRUE)
# max = 58,4

df_trends_merged <- left_join(df_ggtrends_trend, df_snds_trend, by = "Date")

df_trends_merged <- df_trends_merged %>% 
  rename("trend_google" = trend.x, "trend_snds" = trend.y)

coeff <- 2300

ggplot(df_trends_merged, aes(x=Date)) +
  
  geom_line( aes(y=`trend_google`), color = "#882E72") + 
  geom_line( aes(y=`trend_snds` / coeff), color = "#F1932D") + 
  
  scale_y_continuous(
    name = "trend of HPV related research intensity",
    limits = c(0, 100),
    sec.axis = sec_axis(~.*coeff, name="trend of HPV dispensations")
  ) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
  theme(
    axis.title.y = element_text(color = "#882E72", size=13),
    axis.title.y.right = element_text(color = "#F1932D", size=13)
  ) +
  
  ggtitle("Trends of HPV related google searches and HPV vaccines dispensations")

# calcul de la correlation

df_trends_merged <- na.omit(df_trends_merged)
cor.test(df_trends_merged$trend_google, df_trends_merged$trend_snds, method = "pearson")
cor.test(df_trends_merged$trend_google, df_trends_merged$trend_snds, method = "spearman")
