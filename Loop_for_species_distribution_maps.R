############################################################################### 
##RLS distibution maps#########################################################
# By Jac Monk################################################################## 
# jacquomo.monk@utas.edu.au####################################################

## Clear workspace
rm(list=ls())

#Load libraries
library(rgdal)
library(tidyverse)
library(ggplot2)

setwd("C:\\GitHub\\RLS_australian_distribution_maps\\Data")#change to suit
shape.path = "C:\\GitHub\\RLS_australian_distribution_maps\\Data"

##Read in shapefile for background on map
ausply <- readOGR(dsn = shape.path, layer = "land_100k")


##Reproject shape for ggplot()
proj4string(ausply)
ausply <- spTransform(ausply, CRS("+proj=longlat +datum=WGS84"))

#Read in species data
dat <- read.csv("ausspecies.csv")%>%
  rename(species = ï..species)%>%
  drop_na()%>% #get rid of any NA data 75 na values
  glimpse()#load species abundance file

#Get a list of all unique species
species <- unique(dat$scientific)

#Set up base map of australia
p<- ggplot()+geom_polygon(data=ausply,aes(x=long,y=lat, group=group)
                          ,color = "darkgrey", #set outline colour 
                          fill="lightgrey")+ # set fill colour
             xlim(105,160)+
             ylim(-45,-9)+
             theme_classic()

#get rid of all text
p<- p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),legend.position="none",
      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),plot.background=element_blank())
#Check that base map is plotting ok
plot(p)

#test with one species
species <- c("Abudefduf sexfasciatus")#,

#Set output directory
setwd("C:\\GitHub\\RLS_australian_distribution_maps\\Plots")#change to suit

# create for loop to produce ggplot2 maps using aus base map and add species abundance points 
for (i in 1:length(species)) { 
 
  final.plot <- p+ geom_point(data=dat[dat$species == species[i], ],
                              colour = "red",size=4,
                              aes(x=long, y=lat))+#,
                              #size=4, 
                              #col= "FF0000"))#, alpha = 0.5)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  tiff(paste0(species[i], ".tiff"),width = 3200, height = 2700, units = "px", res = 300,
               compression = "lzw")
  print(final.plot)
  dev.off()
}  
  
  

