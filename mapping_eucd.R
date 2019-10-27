#library(OpenStreetMap)
library(raster)
#library(mapplots)
library(rgdal)
#library(plotrix)
library(maps)
library(sp)
library(raster) 
library(RColorBrewer)
library(rgeos)

## Import collared dove points 841
read.csv("coded_final.csv",header=T,stringsAsFactors=F) -> coded

# Get 95 flocks in flight
coded_fif <- coded %>% filter(FiF==1)

#Import pelagic
eucd_pelagic <- readRDS("eucd_pelagic.rds")

#Define projections
latlong_proj <- "+proj=longlat +datum=WGS84 +no_defs"
utm10_proj <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

## Import borders
can <- readOGR("/Users/dave/crows/ms - hybrid zone/final figures/crow_map_final/gis_new/province",layer="province")
usa <- readOGR("/Users/dave/crows/ms - hybrid zone/final figures/crow_map_final/gis_new/gz_2010_us_040_00_500k",layer="gz_2010_us_040_00_500k")
mex <- readOGR("/Users/dave/crows/ms - hybrid zone/final figures/crow_map_final/gis_new/MEX_adm",layer="MEX_adm1")

can <- spTransform(can,utm10_proj)
usa <- spTransform(usa,utm10_proj)
mex <- spTransform(mex,utm10_proj)

#The 834 coastal with species comments
points <- SpatialPoints(coords=cbind(coded$longitude,coded$latitude),proj4string=crs(latlong_proj))
points <- spTransform(points,utm10_proj)

#The 95 coastal noting flying flocks
points95 <- SpatialPoints(coords=cbind(coded_fif$longitude,coded_fif$latitude),proj4string=crs(latlong_proj))
points95 <- spTransform(points95,utm10_proj)

#The pelagics
points_pel <- SpatialPoints(coords=cbind(eucd_pelagic$longitude,eucd_pelagic$latitude),proj4string=crs(latlong_proj))
points_pel <- spTransform(points_pel,utm10_proj)


## Manual encode UTM-zone 10 boundaries for 2:1 aspect ratio Western NA map
x0 <- -426386.5
x1 <- 1232086
y0 <- 3466694
y1 <- 6783638

## Crop to desired extent
#points <- crop(points, extent(x0, x1, y0, y1))
#can <- crop(can, extent(x0, x1, y0, y1))
#usa <- crop(usa, extent(x0, x1, y0, y1))

pdf("map_coastal834_95_pelagic.pdf",2.5,5)
par(mar=rep(0,4))
plot(1, type="n", xlab="", ylab="",xaxs = "i", yaxs = "i",xlim=c(x0, x1), ylim=c(y0, y1))
lines(usa,col='gray60')
lines(can,col='gray60')
lines(mex,col='gray60')
plot(points,add=T,bg='transparent',col='black',pch=21)
plot(points95,add=T,bg='transparent',col='#CC3311',pch=21)
plot(points_pel,add=T,bg='transparent',col='#0077BB',pch=21)
dev.off()
# 
# pdf("map_coastal95.pdf",2.5,5)
# par(mar=rep(0,4))
# plot(1, type="n", xlab="", ylab="",xaxs = "i", yaxs = "i",xlim=c(x0, x1), ylim=c(y0, y1))
# lines(usa,col='gray60')
# lines(can,col='gray60')
# lines(mex,col='gray60')
# plot(points95,add=T,bg='transparent',col='red',pch=21)
# dev.off()


pdf("map_pelagic.pdf",2.5,5)
par(mar=rep(0,4))
plot(1, type="n", xlab="", ylab="",xaxs = "i", yaxs = "i",xlim=c(x0, x1), ylim=c(y0, y1))
lines(usa,col='gray60')
lines(can,col='gray60')
lines(mex,col='gray60')
plot(points_pel,add=T,bg='transparent',col='blue',pch=21)
dev.off()
