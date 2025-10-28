
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALYSIS OF SATELLITE TRACKING DATA FROM TWO BLACK-CAPPED PETRELS, 2019 #
# 1. Track modelling using package aniMotum (Jonsen et al. 2023)  # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Written by Yvan Satgé
## Clemson University - South Carolina Cooperative Fish and Wildlife Research Unit
## Finalized 2025-10-28
## R version 4.4.1 (2024-06-14)



# ---- Packages -----------------------------------------------------------

# install.packages("aniMotum",
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = TRUE)

# https://ianjonsen.r-universe.dev/aniMotum/doc/manual.html#aniMotum-package
# https://ianjonsen.github.io/aniMotum/articles/Overview.html

library(aniMotum)
library(move2)
library(plyr)
library(dplyr)
library(tidyr)



# Load data ---------------------------------------------------------------

# https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study746910348
df<-movebank_download_study(746910348, sensor_type_id = "argos-doppler-shift", 
                            attributes = "all")

# Turn m into a dataframe with lat/lon columns
# Function select() must be used first
df <- df %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(individual_local_identifier, timestamp, argos_lc, 
         lon, lat, argos_semi_major, argos_semi_minor, argos_error_radius) %>% 
  dplyr::rename(id = individual_local_identifier,
                date = timestamp, lc = argos_lc, 
                smaj = argos_semi_major, smin = argos_semi_minor, 
                eor = argos_error_radius)

df$smaj <- as.double(df$smaj)
df$smin <- as.double(df$smin)
df$eor <- as.double(df$eor)



# Model tracks using Random-Walk ---------------------------------------------------------

# Check mean time between two consecutive PTT locations to inform Random Walk parameters
d<-df
d <- d %>%
  plyr::arrange(id, date) %>%
  group_by(id) %>%
  mutate(diff = round(as.numeric(strptime(date, "%Y-%m-%d %H:%M:%S") - 
                                   lag(strptime(date, "%Y-%m-%d %H:%M:%S"), 
                                       default = strptime(date, "%Y-%m-%d %H:%M:%S")[1]),
                                 units = 'mins')))
# 6*60: tags are ON for 6 hours, so maximum "normal" gap should be ~8hrs
d[d$diff< 6*60,] %>% ddply(., ~id, summarise, 
                           mean = round(mean(diff),2))

mean(d$diff[d$diff<6*60] )
# The mean period between two consecutive PTT locations is 33.5 minutes 
# => Use time.step of 30 minutes

## Run (non-correlated) Random-walk model
# Use vmax = 25m/s (90km/h)
fit <- fit_ssm(df, model = "rw", time.step = .5, vmax = 25, control = ssm_control(verbose = 0))

# Correlated Random-walk model estimates nonsensical movements when data gaps are too long
# e.g. around Hispaniola (see bottom of script)
# => Keep (non-correlated) Random Walk instead

# plot lat, lon: observed (blue) and predicted (orange)
plot(fit[fit$id=="HA06",], what = "predicted", type = 1)
# plot track: observed (blue) and predicted (orange)
plot(fit[fit$id=="HA06",], what = "predicted", type = 2)

# Show fit values for first bird
fit[["ssm"]][[1]]

# Get model-predicted locations, and standard errors for these locations
fit.df<-as.data.frame(grab(fit, as_sf = FALSE, what = "p"))


## Filter fitted data
# Some studies have filtered locations to keep only the fitted locations within
# 1-2h of actual PTT locations
# Instead, we decided to keep only predicted locations with x.se and y.se within
# 95% percentile of error radius of location classes 0-3 (i.e. 10277m)
q<-quantile(df[df$lc %in% c("0", "1", "2", "3"),]$eor, 
            c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95, .99, 1))
q
# 95% percentile: 10.3 km

fit.df95<-fit.df[fit.df$x.se<q[10]/1000& 
                   fit.df$y.se<q[10]/1000,]
write.csv(fit.df95, "Data/RW_20251028.csv", row.names = FALSE)



# Miscellaneous statistics -------------------------------------------------------------------

# Location classes

df %>% ddply(.(lc), summarize, 
             n = length(lc), 
             pct = round(length(lc)/length(df$date)*100,2),
             error_mean = round(mean(eor),2),
             error_min = min(eor),
             error_max = max(eor))



quantile(fit.df95$x.se)
mean(fit.df95$x.se)
median(fit.df95$x.se)
sd(fit.df95$x.se)



# leaflet map -------------------------------------------------------------

library(leaflet)
library(leafpop)

x = "HA06"
y <- fit.df95[fit.df95$id==x,]

leaflet(df[df$id==x,]) %>% 
  addTiles() %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   radius = 3, weight = 1, color = "black",
                   popup = popupTable(df[df$id==x,]), group = "data") %>%
  addPolylines(lng = ~lon, lat = ~lat,
               weight = 1, color = "black", group = "data") %>%
  
  addCircleMarkers(lng = df[df$id==x & df$lc %in% c("3", "2","1"),]$lon, 
                   lat = df[df$id==x & df$lc %in% c("3", "2","1"),]$lat,
                   radius = 3, weight = 1, color = "black",
                   popup = popupTable(df[df$id==x & df$lc %in% c("3", "2","1"),]), 
                   group = "LC=3,2,1") %>%
  addPolylines(lng = df[df$id==x & df$lc %in% c("3", "2","1"),]$lon, 
               lat = df[df$id==x & df$lc %in% c("3", "2","1"),]$lat,
               weight = 1, color = "black", group = "LC=3,2,1") %>%
  
  addPolylines(lng = fit.df[fit.df$id==x,]$lon, lat = fit.df[fit.df$id==x,]$lat,
               weight = 1, color = "green", group = "fit") %>%
  addCircles(lng = fit.df[fit.df$id==x,]$lon, lat = fit.df[fit.df$id==x,]$lat,
             radius = 2, weight = 1, color = "green",
             popup = ~popupTable(fit.df[fit.df$id==x,]), group = "fit") %>%
  
  addPolylines(lng = y$lon, 
               lat = y$lat,
               weight = 1, color = "red", group = "fit95") %>%
  addCircles(lng = y$lon, 
             lat = y$lat,
             radius = ~y$x.se*1000, weight = 1, color = "red",
             popup = ~popupTable(y), group = "fit95") %>%
  
  addLayersControl(overlayGroups = c("data", "LC=3,2,1", "fit", "fit95"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addMeasure(primaryLengthUnit = "kilometers", position = "bottomleft")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # #    END OF SCRIPT 1_Track modelling.R    # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #