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

fread('ebd_sampling_relOct-2018/ebd_sampling_relOct-2018.txt',drop="TRIP COMMENTS") -> effort
names(effort) <- names(effort) %>% tolower %>% gsub("[/ ]","_",.)
effort <- effort %>%
  filter(state_code %in% c("US-AK","CA-BC","US-WA","US-OR","US-CA")) %>%
  filter(state != "Washington" | county %in% c("San Juan","Island","Whatcom","Skagit","Snohomish","King","Pierce","Thurston","Mason","Kitsap","Clallam","Jefferson","Grays Harbor","Pacific","Wahkiakum")) %>%
  filter(state != "Oregon" | county %in% c("Columbia","Clatsop","Tillamook","Lincoln","Lane","Douglas","Coos","Curry")) %>%
  filter(state != "California" | county %in% c("Del Norte","Humboldt","Mendocino","Sonoma","Marin","Napa","Solano","Contra Costa","Alameda","Santa Clara","San Mateo","San Francisco","Santa Cruz","Monterey","San Luis Obispo","Santa Barbara","Ventura","Los Angeles","Orange","San Diego"))
effort <- effort %>% collapse_shared
effort <-
  effort %>% select(last_edited_date,country_code,state_code,county,locality_id,latitude,longitude,observation_date,observer_id,sampling_event_identifier,protocol_type,protocol_code,all_species_reported,group_identifier)
write.csv(effort,"effort_stateprovcty_column_filtered.csv")

### On MacBook ###

load_effort_stateprovcty_column_filtered <- function(){
fread("effort/effort_stateprovcty_column_filtered.csv") %>%
  filter(get_year(observation_date) != 2018 | get_month(observation_date) < 9) %>% # Omit observations after August 2018, to match effort dataset to EUCD coded dataset
  filter(get_year(observation_date) > 2006) # Use records 2007 and newer, since species comments were introduced in June 2006
}

latlong_proj <- "+proj=longlat +datum=WGS84 +no_defs"
utm10_proj <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

### READ IN OCEAN DATA

ocean_shp <- readOGR("ne_10m_ocean",layer="ne_10m_ocean")
crs(ocean_shp)
extent(ocean_shp)

## Make table of distinct localities

load_effort_stateprovcty_column_filtered() -> effort
nrow(effort)
#effort %>% filter(get_year(observation_date) == 2018) %>% use_series(observation_date) %>% get_month %>% range
#effort %>% filter(get_year(observation_date) != 2018) %>% use_series(observation_date) %>% get_month %>% range

loctable <- effort %>% distinct(locality_id,.keep_all=T)
rm(effort)
xy <- loctable[,c('longitude','latitude')]
#spdf <- SpatialPointsDataFrame(coords = xy,
#                               data = eucd,
#                               proj4string = crs(latlong_proj))
sps <- SpatialPoints(coords = xy,proj4string = crs(latlong_proj))
rm(xy)
#spdf <- spTransform(spdf,crs(ocean_shp))
#spdf <- spTransform(spdf,utm10_proj)
#sps <- as(spdf,'SpatialPoints')
crs(sps)
extent(sps)
sps <- spTransform(sps,utm10_proj)
crs(sps)
extent(sps)

nrow(loctable) # Number of distinct LocIDs

## Uncomment this section if making new ocean raster, otherwise readRDS, at bottom:
#Sys.time()
#r <- raster(extent(sps),
#       nrows=(extent(sps)@ymax-extent(sps)@ymin)/2000,
#       ncols=(extent(sps)@xmax-extent(sps)@xmin)/2000,
#       crs=crs(sps),
#       vals=NA)
#r <- projectRaster(from=r,crs=crs(ocean_shp))
## Now I have a raster of NAs, at the extent of EUCD, with Ocean projection
#Sys.time()
#r <- rasterize(ocean_shp, r) #Takes 4 minutes
#rm(ocean_shp)
#saveRDS(r,"raster_2000m_latlong.rds")
readRDS("raster_2000m_latlong.rds") -> r

Sys.time()

## Extract whether point is <5 miles from Ocean, 1000 at a time, takes ~ 30 min
## Uncomment to use this by-1000-at-a-time version, otherwise use all-at-once version below
#dest <- rep(NA,length(sps))
#sps.index <- 1:length(sps)
#split.factor.index <- ceiling(seq_along(sps.index)/1000)
#d_by_1000 <- split(sps.index, split.factor.index)
#for (i in seq_along(d_by_1000)){
#  dest[d_by_1000[[i]]] <- extract(r, spTransform(sps[d_by_1000[[i]]], latlong_proj), buffer=8047, fun=function(x){1 %in% x}, na.rm=F)
#  print(i*1000/length(sps)*100)
#}
#loctable$near_ocean <- dest
#rm(dest,i,split.factor.index,sps.index)

## Extract whether point is <5 miles from Ocean
## All at once version (takes ~30 min on effort dataset)
  loctable$near_ocean <-
  extract(r,spTransform(sps,latlong_proj),
          buffer=8047,fun=function(x){1 %in% x},na.rm=F)

Sys.time()

#saveRDS(loctable,"loctable.rds")
loctable %>% nrow  #  466,064 unique localities for effort
#readRDS("loctable.rds")

load_effort_stateprovcty_column_filtered() -> effort

## Add "near_ocean" field to main effort data frame
effort$near_ocean <-
  left_join(effort,loctable,by="locality_id") %>% use_series(near_ocean)

effort_coastal <- 
  effort %>% filter(near_ocean==TRUE)
#saveRDS(effort_coastal,file="effort_coastal.rds")
#readRDS("eucd_coastal.rds") -> eucd_coastal

nrow(effort_coastal)

######  BEGIN PELAGIC EFFORT FILTERING ######

# Read land shapefile

land_shp <- readOGR("ne_10m_land",layer="ne_10m_land")
crs(land_shp)
extent(land_shp)


Sys.time()
## Make raster of NAs, with Land_shp projection
r <- raster(extent(sps),
            nrows=(extent(sps)@ymax-extent(sps)@ymin)/2000,
            ncols=(extent(sps)@xmax-extent(sps)@xmin)/2000,
            crs=crs(sps),
            vals=NA)
r <- projectRaster(from=r,crs=crs(land_shp))

Sys.time()

r <- rasterize(land_shp, r) #Takes a while

Sys.time()

## Efficiently extract whether point is >5 miles from land, Takes 25 minutes
loctable$pelagic <-
  extract(r,spTransform(sps,latlong_proj),
          buffer=8047,fun=function(x){all(is.na(x))},na.rm=F)
#rm(land_shp,sps)
#rm(r)
#rm(latlong_proj,utm10_proj)
Sys.time()


## Add "pelagic" field to effort data frame
load_effort_stateprovcty_column_filtered() -> effort
effort$pelagic <-
  left_join(effort,loctable,by="locality_id") %>% use_series(pelagic)
#rm(loctable)

effort_pelagic <- 
  effort %>% filter(pelagic==TRUE)
#saveRDS(effort_pelagic,file="effort_pelagic.rds")

