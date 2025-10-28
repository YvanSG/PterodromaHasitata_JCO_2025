
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALYSIS OF SATELLITE TRACKING DATA FROM TWO BLACK-CAPPED PETRELS, 2019 #
# 3. Figure 2   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Written by Yvan Satgé
## Clemson University - South Carolina Cooperative Fish and Wildlife Research Unit
## Finalized 2025-10-28
## R version 4.4.1 (2024-06-14)



# ---- Packages -----------------------------------------------------------

library(leaflet)
library(leaflet.esri)
library(leafpop)
library(ggplot2)


# Maps --------------------------------------------------------------------

# Use leaflet for maps

factpal <- colorFactor(rainbow(10), 2)
id442<-fit.df95[fit.df95$id=="HA08",]
id462<-fit.df95[fit.df95$id=="HA06",]

leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = FALSE) %>% 
  addCircleMarkers(id442, lng = id442$lon, lat = id442$lat, 
                   stroke = FALSE, radius = 3, fillOpacity = .75, 
                   fillColor = "blue", group = "442", popup = popupTable(id442)) %>%
  addPolylines(id442, 
               lng = id442$lon, 
               lat = id442$lat, 
               stroke = TRUE, weight = 1.5, color = "blue", group = "442") %>%
  
  addCircleMarkers(fit.df95[fit.df95$id=="HA10",], 
                   lng = id462$lon, 
                   lat = id462$lat, 
                   stroke = FALSE, radius = 3, fillOpacity = .75, 
                   color = "yellow", group = "462", popup = popupTable(id462)) %>%
  addPolylines(id462, 
               lng = id462$lon, 
               lat = id462$lat, opacity = 1,
               stroke = TRUE, weight = 1.5, color = "yellow", group = "462") %>%
  
  addLayersControl(overlayGroups = c("442", "462"), 
                   position = "topleft", options = layersControlOptions(collapsed = F)) %>% 
  addMeasure("bottomright", primaryLengthUnit = "km")



# Plots -------------------------------------------------------------------

# Petrel HA06 - 181462 (Geotrak) ----------------------------------------------------------

HA06<-bat[as.Date(bat$timestamp)>as.Date("2019-09-01")&
            as.Date(bat$timestamp)<as.Date("2019-11-15")&
            bat$id=="HA06",]
HA06D<-batD[as.Date(batD$day)>as.Date("2019-09-01")&
              as.Date(batD$day)<as.Date("2019-11-15")&
              batD$id=="HA06",]

# Distance to nearest nesting area: Valle Nuevo (-70.5984, 18.6452)
# Use Random-walk modelled locations
HA06dist<-fit.df95[as.Date(fit.df95$date)>as.Date("2019-09-01")&
                     as.Date(fit.df95$date)<as.Date("2019-11-15")&
                     fit.df95$id=="HA06",]

# Calculate distance between track data and Valle Nuevo
HA06dist$dist<-distGeo(HA06dist[,c(3,4)], c(-70.5984, 18.6452))/1000

# Calculate basic voltage statistics
HA06bat<-bat$voltage[bat$day>as.Date("2019-09-01") & 
                       bat$day<as.Date("2019-11-15") & 
                       bat$id=="HA06"]
quantile(HA06bat, 0.1) # 3.8
mean(HA06bat) # 3.902857


## Create plots for Figure 1 - Axis titles will be added later
# Create a data frame with timestamps of expected satellite communications
HA06ptt<-data.frame(start = seq.POSIXt(as.POSIXct('2019-09-02 19:45:01', 
                                                  format = '%Y-%m-%d %H:%M:%S'), 
                                       as.POSIXct('2019-11-14 22:30:00', 
                                                  format = '%Y-%m-%d %H:%M:%S'),
                                       by = '34 hours'), 
                    end = seq.POSIXt(as.POSIXct('2019-09-03 01:45:01', 
                                                format = '%Y-%m-%d %H:%M:%S'),
                                     as.POSIXct('2019-11-14 22:30:00', 
                                                format = '%Y-%m-%d %H:%M:%S'), 
                                     by = '34 hours'),
                    ymax = Inf,
                    ymin = -Inf)


HA06v1<-geom_rect(data = data.frame(start = as.POSIXct('2019-10-02', format = '%Y-%m-%d'), 
                                    end = as.POSIXct('2019-10-08', format = '%Y-%m-%d'),
                                    ymax = Inf,
                                    ymin = -Inf),
                  aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                  fill = "yellow", alpha = 0.4)
HA06v2<-geom_rect(data = data.frame(start = as.POSIXct('2019-10-09', format = '%Y-%m-%d'), 
                                    end = as.POSIXct('2019-10-15', format = '%Y-%m-%d'),
                                    ymax = Inf,
                                    ymin = -Inf),
                  aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                  fill = "yellow", alpha = 0.4)


HA06v1d<-geom_rect(data = data.frame(start = as.POSIXct('2019-10-02', format = '%Y-%m-%d'), 
                                     end = as.POSIXct('2019-10-08', format = '%Y-%m-%d'),
                                     ymax = Inf,
                                     ymin = 0),
                   aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                   fill = "yellow", alpha = 0.4)
HA06v2d<-geom_rect(data = data.frame(start = as.POSIXct('2019-10-09', format = '%Y-%m-%d'), 
                                     end = as.POSIXct('2019-10-15', format = '%Y-%m-%d'),
                                     ymax = Inf,
                                     ymin = 0),
                   aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                   fill = "yellow", alpha = 0.4)

HA06s<-scale_x_datetime(date_breaks = "1 week", 
                        labels=date_format('%d %b'),
                        limits = c(as.POSIXct('2019-09-01', format = '%Y-%m-%d'),
                                   as.POSIXct('2019-11-15', format = '%Y-%m-%d')),
                        expand = c(0,0))



g<-ggplot()+geom_point(data = HA06, aes(x = timestamp, y = id), size = 0.5)+
  theme(axis.text.x  = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.x = element_blank(),
        plot.margin=unit(c(0.1,0.1,0,0),"in"))+ 
  scale_y_discrete(labels = "462")+ ylab(element_blank())+
  HA06v1 + HA06v2 + HA06s +
  geom_rect(data = HA06ptt,
            aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.2) 

h<-ggplot()+
  geom_hline(yintercept=(28/2*23.9), linetype="dashed", color = "red", linewidth = 0.2) +
  geom_point(data = HA06dist, aes(x = date, y = dist), size = 0.5)+
  geom_line(data = HA06dist, aes(x = date, y = dist), linewidth = 0.3)+
  theme(axis.text.x  = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0,0.1,0,0),"in"))+ 
  ylab(element_blank())+ scale_y_continuous(trans='log10') +
  HA06v1d + HA06v2d + HA06s

i<-ggplot()+
  geom_hline(yintercept=mean(HA06bat), color = "red", linewidth = 0.2) +
  geom_hline(yintercept=quantile(HA06bat, 0.1), linetype="dashed", 
             color = "red", linewidth = 0.2) +
  geom_line(data=HA06D, aes(day, meanV), linewidth = 0.3) + 
  geom_point(data=HA06D, aes(day, meanV), size = 0.5) +
  theme(axis.text.x  = element_text(angle=90, hjust = 1, vjust = 0.5),
        plot.margin=unit(c(0,0.1,0,0),"in"))+ 
  ylab(element_blank())+ xlab(element_blank()) + HA06v1 + HA06v2 + HA06s

g06<-ggarrange(g, h, i + font("x.text", size = 8),heights = c(0.5,1, 2),
               ncol = 1, nrow = 3, align = "v")

# Petrel HA08-174442 (Microwave) ------------------------------------------------------
# 2019-10-15 / 2019-12-29

HA08<-bat[as.Date(bat$timestamp)>as.Date("2019-10-15")&
            as.Date(bat$timestamp)<as.Date("2019-12-29")&
            bat$id=="HA08",]

HA08D<-batD[as.Date(batD$day)>as.Date("2019-10-15")&
              as.Date(batD$day)<as.Date("2019-12-29")&
              batD$id=="HA08",]

# Distance to nearest nesting area: La Visite (18.3506, -72.2228)
# Use Random-walk modelled locations
HA08dist<-fit.df95[as.Date(fit.df95$date)>as.Date("2019-10-15")&
                     as.Date(fit.df95$date)<as.Date("2019-12-29")&
                     fit.df95$id=="HA08",]

# Calculate distance between track data and La Visite
HA08dist$dist<-distGeo(HA08dist[,c(3,4)], c(-72.2228, 18.3506))/1000

# Calculate basic voltage statistics
mean(bat$voltage[bat$day < as.Date(c("2019-09-01")) & bat$id=="HA08"])
mean(bat$voltage[bat$day>as.Date("2019-12-20") & bat$day<as.Date("2020-01-25") & 
                   bat$id=="HA08"])

HA08bat<-bat$voltage[bat$day>as.Date("2019-10-15") & bat$day<as.Date("2020-12-29") & 
                       bat$id=="HA08"]
quantile(HA08bat, 0.1)
mean(HA08bat)

## Create plots for Figure 1
# Create a data frame with timestamps of expected satellite communications
# 48h+5h = 53h difference between every slot
HA08ptt<-data.frame(start = seq.POSIXt(as.POSIXct('2019-10-15 23:42:23', 
                                                  format = '%Y-%m-%d %H:%M:%S'), 
                                       as.POSIXct('2019-12-29 22:30:00', 
                                                  format = '%Y-%m-%d %H:%M:%S'),
                                       by = '53 hours'), 
                    end = seq.POSIXt(as.POSIXct('2019-10-16 05:42:23', 
                                                format = '%Y-%m-%d %H:%M:%S'), 
                                     as.POSIXct('2019-12-29 22:30:00', 
                                                format = '%Y-%m-%d %H:%M:%S'),
                                     by = '53 hours'),
                    ymax = Inf,
                    ymin = -Inf)

HA08v1<-geom_rect(data = data.frame(start = as.POSIXct('2019-11-10', format = '%Y-%m-%d'), 
                                    end = as.POSIXct('2019-11-21', format = '%Y-%m-%d'),
                                    ymax = Inf,
                                    ymin = -Inf),
                  aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                  fill = "blue", alpha = 0.25)

HA08v2<-geom_rect(data = data.frame(start = as.POSIXct('2019-11-29 12:00:00', 
                                                       format = '%Y-%m-%d %H:%M:%S'), 
                                    end = as.POSIXct('2019-12-03', format = '%Y-%m-%d'),
                                    ymax = Inf,
                                    ymin = -Inf),
                  aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                  fill = "blue", alpha = 0.25)


HA08v1d<-geom_rect(data = data.frame(start = as.POSIXct('2019-11-10', format = '%Y-%m-%d'), 
                                     end = as.POSIXct('2019-11-21', format = '%Y-%m-%d'),
                                     ymax = Inf,
                                     ymin = 0),
                   aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                   fill = "blue", alpha = 0.25)
HA08v2d<-geom_rect(data = data.frame(start = as.POSIXct('2019-11-29 12:00:00', 
                                                        format = '%Y-%m-%d %H:%M:%S'), 
                                     end = as.POSIXct('2019-12-03', format = '%Y-%m-%d'),
                                     ymax = Inf,
                                     ymin = 0),
                   aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
                   fill = "blue", alpha = 0.25)

HA08s<-scale_x_datetime(date_breaks = "1 week", 
                        labels = date_format('%d %b'),
                        limits = c(as.POSIXct('2019-10-15', format = '%Y-%m-%d'),
                                   as.POSIXct('2019-12-29', format = '%Y-%m-%d')),
                        expand = c(0,0))


j<-ggplot()+geom_point(data = HA08, aes(x = timestamp, y = id), size = 0.5)+
  ylab("PTT")+ scale_y_discrete(labels = "442")+
  theme(axis.text.x  = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0.1,0,0,0.1),"in"))+ 
  geom_rect(data = HA08ptt,
            aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.2) + HA08v1 + HA08v2 + HA08s

k<-ggplot()+
  geom_hline(yintercept=(48/2*23.9), linetype="dashed", color = "red", linewidth = 0.2) +
  geom_point(data = HA08dist, aes(x = date, y = dist), size = 0.5)+
  geom_line(data = HA08dist, aes(x = date, y = dist), linewidth = 0.3)+
  theme(axis.text.x  = element_blank(), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0,0,0,0.1),"in"))+ 
  ylab("Distance (km)") + HA08v1d + HA08v2d + HA08s +
  scale_y_continuous(trans='log10')

l<-ggplot()+
  geom_hline(yintercept=mean(HA08bat), color = "red", linewidth = 0.2) +
  geom_hline(yintercept=quantile(HA08bat, 0.1), linetype="dashed", 
             color = "red", linewidth = 0.2) +
  geom_line(data=HA08D, aes(day, meanV), linewidth = 0.3) + 
  geom_point(data=HA08D, aes(day, meanV), size = 0.5) +
  theme(axis.text.x  = element_text(angle=90, hjust = 1, vjust = 0.5),
        plot.margin=unit(c(0,0,0,0.1),"in"))+ 
  HA08v1 + HA08v2 + HA08s +
  ylab("Voltage (v)") + xlab(element_blank())

g08<-ggarrange(j, k, l + font("x.text", size = 8),heights = c(0.5,1, 2),
               ncol = 1, nrow = 3, align = "v")

# Arrange both sets of plots side by side
ggarrange(g08, g06, ncol =2, nrow =1, align = "h")

ggsave("Colony_plot.pdf",
       width = 8.1, height = 3.4, units = "in")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # #    END OF SCRIPT 3_Figure 2.R    # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #