library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(dplyr)
library(usethis)
library(devtools)
# Windows users will likely need to set this
# Sys.setenv('R_REMOTES_NO_ERRORS_FROM_WARNINGS' = 'true')
devtools::install_github("nwfsc-timeseries/atsalibrary")

devtools::install_github("depmix/depmixS4")
library(depmixS4)
devtools::install_github("atsa-es/atsalibrary")
library(atsalibrary)

#### EXEMPLE

## get the matching years of sunspot data
suns <- ts.intersect(lynx, sunspot.year)[, "sunspot.year"]
## get the matching lynx data
lynx <- ts.intersect(lynx, sunspot.year)[, "lynx"]

## plot time series
plot(cbind(suns, lynx), yax.flip = TRUE)

## CCF of sunspots and lynx
ccf(suns, log(lynx), ylab = "Cross-correlation")

### GOOGLE TRENDS = x et SNDS = y 

## Donées SNDS

df_snds <- read.csv("/Users/charlottederogis/Desktop/Stage/données\ SNDS/table_hpv_dose_periode.csv")

df_snds <- df_snds %>% 
  mutate(total = dose_1 + dose_2 + dose_3)

df_snds$Date <- as.Date(paste0("01/", df_snds$periode), format = "%d/%m/%Y")

df_snds <- df_snds %>% 
  dplyr::select(Date, total)

## Données google trends

df_ggtrends <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/CSV/google\ trends/ggt\ total\ dates\ snds.csv")

df_ggtrends$Date <- as.Date(paste0(df_ggtrends$Date, "-01"), format = "%Y-%m-%d")

# transformation en time series

ts_gt <- ts(df_ggtrends$n, start = c(2007, 07), frequency = 12)  # ou frequency = 1 si annuel
ts_snds <- ts(df_snds$total, start = c(2007, 07), frequency = 12)


# même dates
gt <- ts.intersect(ts_gt, ts_snds)[, "ts_gt"]
snds <- ts.intersect(ts_gt, ts_snds)[, "ts_snds"]

## plot time series
plot(cbind(gt, snds), yax.flip = TRUE)

# CCF 
ccf(diff(gt), diff(snds), ylab = "Cross-correlation")
ccf(diff(snds), diff(gt), ylab = "Cross-correlation")

## en metant le signal SNDS sur une echelle de 0 à 100
# y_transforme(t) = [y_original(t) - ymin] / [ymax - ymin])

snds_norm <- (snds - min(snds, na.rm = TRUE)) / (max(snds, na.rm = TRUE) - min(snds, na.rm = TRUE))

ccf(diff(gt), diff(snds_norm), ylab = "Cross-correlation")
ccf(diff(snds_norm), diff(gt), ylab = "Cross-correlation")

## En utilisant les signaux standardisés pour les deux séries 
## (pour cela, utiliser la formule 
##y_transforme(t) = [y_original(t) - moyenne de y_original sur la periode]/[ecart-type de y_original])

snds_stand <- (snds - mean(snds, na.rm = TRUE)) / sd(snds, na.rm = TRUE) 
gt_stand <- (gt - mean(gt, na.rm = TRUE)) / sd(gt, na.rm = TRUE)

ccf(diff(gt_stand), diff(snds_stand) , ylab = "Cross-correlation")
ccf(diff(snds_norm), diff(gt), ylab = "Cross-correlation")
