
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALYSIS OF SATELLITE TRACKING DATA FROM TWO BLACK-CAPPED PETRELS, 2019 #
# 2. Calculations   # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Written by Yvan Satgé
## Clemson University - South Carolina Cooperative Fish and Wildlife Research Unit
## Finalized 2025-10-28
## R version 4.4.1 (2024-06-14)



# ---- Packages -----------------------------------------------------------

library(move2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geosphere)
library(scales)
library(ggpubr)


# Useful information ------------------------------------------------------

# Geotrack: ON during 6h every 34h
# Microwave: ON during 5h every 48h

# Geotrak
# For argos:sensor-1 = 2
# Value of  argos:sensor-2 = 196
# Decode: (2*196) / 100 = 3.92V
# Battery voltage: y = (2 * argos:sensor-2)/100

# Microwave
# Battery conversion (y = voltage, x = sensor2 value)
# y = 3.173 + 0.0049x

# At colony: 
# HA06: between 2019-10-01/2019-10-08; 2019-10-08/2019-10-15 => 2019-09-01 / 2019-11-15 
# HA08: between 2019-11-10/2019-11-22; 2019-11-29   => 2019-10-15 / 2019-12-29



# Load data ---------------------------------------------------------------

# Tracking data (missing tag ID) - turn into dataframe, rename columns
bat<-movebank_download_study(746910348, sensor_type_id = "argos-doppler-shift", 
                                attributes = "all") %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(individual_local_identifier, timestamp, argos_lc, 
         lon, lat, argos_sensor_1, argos_sensor_2, argos_sensor_3, argos_sensor_4) %>% 
  dplyr::rename(id = individual_local_identifier) %>% 
  as.data.frame()

# Reference data (includes tag ID)
ref<-movebank_download_deployment(746910348) %>% 
  dplyr::rename(id = individual_local_identifier, tagid = tag_local_identifier)

# Join tag ID to tracking data
bat<-left_join(bat, ref[,c("id", "tagid")], by = "id")
rm(ref)

# Add information about tag company
bat$co<-NA
bat$co<-ifelse(bat$tagid %in% c("174442", "174441"), "mw", "gt")
bat$tagid<-as.factor(as.character(bat$tagid))
bat$co<-as.factor(as.character(bat$co))

# Calculate battery level using equations provided by tag companies
## Geotrak
batGT<-bat[bat$co=="gt",] %>% droplevels()
# Select only rows where argos sensor 1 has a value of 2
batGT<-batGT[batGT$argos_sensor_1==2,]
# Calculate voltage using value of argos sensor 2
batGT$voltage<-batGT$argos_sensor_2*2/100

## Microwave
batMW<-bat[bat$co=="mw",] %>% droplevels() 
# Looks like there is a number incrementing up to 119
# This is causing issues with appearant battery levels
# Select only rows where argos sensor 2 > 120
batMW<-batMW[batMW$argos_sensor_2>119,] 
# Calculate votage
batMW$voltage<-3.173 + 0.0049*batMW$argos_sensor_2

bat<-rbind(batGT,batMW)
bat<-bat[bat$voltage>=3,] # remove 7 outliers

# Prepare date column
bat$timestamp<-as.POSIXct(as.character(bat$timestamp))
bat$day<-strftime(bat$timestamp, "%Y-%m-%d")
batD<- ddply(bat, .(id, day, co), summarize,
             meanV = mean(voltage))
batD$day<-as.POSIXct(batD$day, format = '%Y-%m-%d')


# Plot changes in battery levels (quick plot)
ggplot()+geom_line(data = batD, aes(as.Date(day), meanV, color = id)) + 
  geom_point(data = batD, aes(as.Date(day), meanV), shape = 1) + 
  scale_x_date(breaks = "1 days") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(data = batD[batD$co=="gt",], aes(as.Date(day), meanV))+
  geom_smooth(data = batD[batD$co=="mw",], aes(as.Date(day), meanV))

# Difference in mean voltage between manufacturers:
# Get normal battery levels in optimal conditions (i.e. at sea)
# => Use only until 2019-08-15
mean(bat$voltage[bat$day < as.Date(c("2019-08-15")) & bat$co=="gt"]) # 4.0v
mean(bat$voltage[bat$day < as.Date(c("2019-08-15")) & bat$co=="mw"]) # 4.1v



# Misc information for manuscript -----------------------------------------

mean(bat$voltage[bat$day>as.Date("2019-09-01") & bat$day<as.Date("2019-11-15") & bat$id=="HA06"])
sd(bat$voltage[bat$day>as.Date("2019-09-01") & bat$day<as.Date("2019-11-15") & bat$id=="HA06"])
quantile(HA06dist$x.se[as.Date(HA06dist$date)>as.Date("2019-09-01") & 
                         as.Date(HA06dist$date)<as.Date("2019-11-15")])
 #### ERRORS WITH THE QUANTILE FUNCTION

mean(bat$voltage[bat$day>as.Date("2019-12-20") & bat$day<as.Date("2020-01-25") & bat$id=="HA08"])
sd(bat$voltage[bat$day>as.Date("2019-12-20") & bat$day<as.Date("2020-01-25") & bat$id=="HA08"])
quantile(HA08dist$dist[as.Date(HA08dist$date)>as.Date("2019-11-29") & 
                         as.Date(HA08dist$date)<as.Date("2019-12-29")])

mean(HA08dist$dist[as.Date(HA08dist$date)==as.Date('2019-12-03', format = '%Y-%m-%d')])


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # #    END OF SCRIPT 2_Calculations.R    # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #