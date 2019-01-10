library(data.table)
library(magrittr)
library(plyr)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)

### Define helper functions

no_s <- function(x){
  require(magrittr)
  sub("S","",x) %>% as.numeric
}

collapse_shared <- function(x){
  require(magrittr)
  require(plyr)
  require(dplyr)
  rbind(
    x %>% filter(group_identifier == "")
    ,
    x %>% filter(group_identifier != "") %>% group_by(group_identifier) %>% filter(no_s(sampling_event_identifier)==sort(no_s(sampling_event_identifier))[1]) %>% ungroup
  )
}

get_month <- function(x){
  require(magrittr)
  substr(x,6,7) %>% as.numeric
}

get_year <- function(x){
  require(magrittr)
  substr(x,1,4) %>% as.numeric
}

### READ IN EUCD DATA

## Define infile path
infile <- "ebd_eucdov_prv_relAug-2018/ebd_eucdov_prv_relAug-2018.txt"

## Read data
eucd <- fread(infile)
names(eucd) <- names(eucd) %>% tolower %>% gsub("[/ ]","_",.)
nrow(eucd) # After read-in # 1,862,997

eucd <- eucd %>%
  filter(state_code %in% c("US-AK","CA-BC","US-WA","US-OR","US-CA")) %>%
  filter(state != "Washington" | county %in% c("San Juan","Island","Whatcom","Skagit","Snohomish","King","Pierce","Thurston","Mason","Kitsap","Clallam","Jefferson","Grays Harbor","Pacific","Wahkiakum")) %>%
  filter(state != "Oregon" | county %in% c("Columbia","Clatsop","Tillamook","Lincoln","Lane","Douglas","Coos","Curry")) %>%
  filter(state != "California" | county %in% c("Del Norte","Humboldt","Mendocino","Sonoma","Marin","Napa","Solano","Contra Costa","Alameda","Santa Clara","San Mateo","San Francisco","Santa Cruz","Monterey","San Luis Obispo","Santa Barbara","Ventura","Los Angeles","Orange","San Diego")) %>%
  filter(approved==1) %>%
  collapse_shared

nrow(eucd) # After state/county filtering # 262,812
saveRDS(eucd,"eucd.rds")
# 
# ### Keyword processing, ALL data!
# 
# eucd <- fread(infile)
# names(eucd) <- names(eucd) %>% tolower %>% gsub("[/ ]","_",.)
# 
# eucd_keywords <- eucd %>%
#   filter(country %in% c("United States","Canada")) %>%
#   filter(species_comments != "") %>%
#   filter(flight_keyword(species_comments))
# # 5304 keyword records

### DEFINE PROJECTIONS

latlong_proj <- "+proj=longlat +datum=WGS84 +no_defs"
utm10_proj <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

### READ IN OCEAN DATA

ocean_shp <- readOGR("ne_10m_ocean",layer="ne_10m_ocean")
crs(ocean_shp)
extent(ocean_shp)
#ocean_shp <- spTransform(ocean_shp,utm10_proj)

## Make table of distinct localities

loctable <- eucd %>% distinct(locality_id,.keep_all=T)
xy <- loctable[,c('longitude','latitude')]
#spdf <- SpatialPointsDataFrame(coords = xy,
#                               data = eucd,
#                               proj4string = crs(latlong_proj))
sps <- SpatialPoints(coords = xy,proj4string = crs(latlong_proj))
#spdf <- spTransform(spdf,crs(ocean_shp))
#spdf <- spTransform(spdf,utm10_proj)
#sps <- as(spdf,'SpatialPoints')
crs(sps)
extent(sps)
sps <- spTransform(sps,utm10_proj)
crs(sps)
extent(sps)

nrow(loctable) # Number of distinct LocIDs # 40,280

#buffered <- gBuffer(spdf,width=10000,byid=T)
#buffered <- SpatialPolygonsDataFrame( buffered, data=buffered@data )
#writeOGR( buffered, "buffered", "buffered", driver="ESRI Shapefile" ) 
#buffered <- spTransform(buffered,crs(ocean_shp))
#over(buffered,ocean_shp)
#gIntersects(buffered,ocean_shp)

Sys.time()
r <- raster(extent(sps),
            nrows=(extent(sps)@ymax-extent(sps)@ymin)/2000,
            ncols=(extent(sps)@xmax-extent(sps)@xmin)/2000,
            crs=crs(sps),
            vals=NA)
r <- projectRaster(from=r,crs=crs(ocean_shp))
## Now I have a raster of NAs, at the extent of EUCD, with Ocean projection

Sys.time()

r <- rasterize(ocean_shp, r) #Takes 4 minutes
#saveRDS(r,"raster_2000m_latlong.rds")
#readRDS("raster_2000m_latlong.rds") -> r

Sys.time()

## Old inefficient code
#r <- projectRaster(from=r,crs=crs(utm10_proj)) ## This distorts the raster
#p = as(r,"SpatialPoints")
#d <- gDistance(p, lines, byid=TRUE)
# 
# system.time({ # Takes about 20 minutes
# rep(NA,length(sps)) -> near_ocean
# for (i in 1:length(sps)) {
#   #localraster <- p[abs(p$x-sps[i]@coords[1,1])<10000 & abs(p$y-sps[i]@coords[1,2])<10000]
#   #if (length(localraster)==0) {return(NA)}
#   #gDistance(sps[i],localraster)# -> dest[i]
#   extract(r,sps[i],buffer=8047) %>% unlist %>% as.integer %>% {1 %in% .} -> near_ocean[i]
#   if (i %% 1000 ==0) {print(i)}
#   }
# near_ocean -> loctable$near_ocean
# })
#write.csv(eucd,"eucd_near_ocean.csv")

Sys.time()

## Efficiently extract whether point is <5 miles from Ocean, Takes 3 minutes
loctable$near_ocean <-
  extract(r,spTransform(sps,latlong_proj),
          buffer=8047,fun=function(x){1 %in% x},na.rm=F)

Sys.time()

saveRDS(loctable,"loctable.rds")
loctable %>% nrow  #  40,280
#readRDS("loctable.rds")

## Add "near_ocean" descriptor to EUCD main data frame
eucd$near_ocean <-
  left_join(eucd,loctable,by="locality_id") %>% use_series(near_ocean)

## Old loop version...
# rep(NA,nrow(eucd)) -> d
# for (i in 1:nrow(eucd)){
#   loctable[loctable$locality_id == eucd$locality_id[i],'near_ocean'] -> d[i]
#   if (i %% 1000 ==0) {print(i)}
# }
# d -> eucd$near_ocean


eucd_coastal <- 
  eucd %>% filter(near_ocean==TRUE)
#saveRDS(eucd_coastal,file="eucd_coastal.rds")