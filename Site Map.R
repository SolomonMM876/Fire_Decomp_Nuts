####site locations####

library(tidyverse)
library(ggplot2)
library(forcats)
library(patchwork)
library(splitstackshape)
library(tidyr)
library(ggrepel)
library(maps)
library(oz)
library(ggmap)
setwd("\\\\ad.uws.edu.au/dfshare/HomesCMB$/90957135/My Documents/ABS_FIRE/Fire_Decomp_Nuts")

Map_Info<-read.csv('Decompisition Site Locations.csv')
Map_Info$Veg_Class= sub(c('Sydney'), '',Map_Info$Veg_Class)
colnames(Map_Info)[1]<-'Site'
Map_Info$Site= sub(c('ABS00'),'',Map_Info$Site)
Map_Info$Site= sub(c('ABS0'), '',Map_Info$Site)

head(Map_Info)



australia_map <- map_data("world", region = "Australia")
australian_states <- map_data("state")


x_limits <- c(148, 152)
y_limits <- c(-35, -32)

p<-ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "black") +
  geom_point(data = Map_Info, aes(x = Longitude, y = Latitude, 
                                  size= Veg_Class,color = Interval,shape=Severity), size = 4) +
  geom_text(data = Map_Info, aes(x = Longitude, y = Latitude,label=Site), size = 2) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +  # Set zoomed-in limits
  scale_color_discrete(name = "Fire Regime") +
  scale_shape_discrete(name="")+
  coord_fixed(ratio = 1) + # Adjust the ratio for a better-looking map
  theme_void()
p
plotly::ggplotly(p)




#map <- get_stamenmap( bbox = c(left = 110, bottom = -40, right = 160, top = -10),                    zoom = 4)
#ggmap(map, extent = 'normal') + 
 # geom_point(data = Map_Info, aes(x = Longitude, y = Latitude, group = as.factor(Veg_Class)), fill = "red")




#NSW
#oz(sections=c(4,13:15))

# this time colour points by 'species' and add legend
#with(Map_Info, points(Longitude, Latitude, pch=16 ,col= as.factor(Interval),size=Severity, cex=0.75))
#legend('topright', levels(as.factor(Map_Info$Veg_Class)), col=palette(), pch=16, cex=0.5)


