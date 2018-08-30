###############################################
##### Plot bunch of tracks #####
###############################################


# load packages ----

library(jsonlite)
library(lubridate)
library(sf)
library(cartography)
library(leaflet)
library(reshape2)
library(igraph)
library(dplyr)

options(scipen = 100000)


# load data ----

allFiles <- list.files(path = "DATA/Records-2017-12-16/")
allNames <- gsub(x = allFiles, pattern = "_|-| |default|.json", replacement = "")
allNamesCorr <- gsub(x = allNames, pattern = "[[:digit:]]", replacement = "")
bboxDormoy <- readRDS(file = "DATA/basemap/bboxdormoy.Rds") %>% st_transform(crs = 2154)
restWorld <- readRDS(file = "DATA/basemap/restworld.Rds")


# extract one JSON ----

listPts <- list()
i = allFiles[[1]]
i = "default_eleonore_20171115-1134.json"
for(i in allFiles){
  oneJson <- fromJSON(txt = paste0("DATA/Records-2017-12-16/", i), 
                      simplifyDataFrame = TRUE,
                      flatten = TRUE)
  if(is.data.frame(oneJson)){
    dataJson <- oneJson
  } else {
    dataJson <- oneJson$data
  }
  
  dataJson$NAME <- i
  dataJson$CONCAT <- paste(dataJson$newValue.x, dataJson$newValue.y, sep = "_")
  dataJson$DUPLI <- !duplicated(dataJson$CONCAT)
  
  onePts <- dataJson[dataJson$DUPLI, ] %>% 
    st_as_sf(agr = "constant", coords = c("newValue.y", "newValue.x"), crs = 4326) %>% 
    st_transform(crs = 2154)
  
  if(nrow(onePts) > 50){
    listPts[[length(listPts) + 1]] <- onePts
  }
}


# compute time interval ----

GetInterval <- function(x){
  initCoords <- st_coordinates(x)
  valTime <- as.vector(x$timestamp)
  difTime <- valTime[2:length(valTime)] - valTime[1:length(valTime)-1]
  xCoords <- mapply(mean, initCoords[1:nrow(initCoords)-1, 1], initCoords[2:nrow(initCoords), 1])
  yCoords <- mapply(mean, initCoords[1:nrow(initCoords)-1, 2], initCoords[2:nrow(initCoords), 2])
  tabInterval <- data.frame(X = xCoords, Y = yCoords, INTERVAL = difTime)
  sfInterval <- st_as_sf(tabInterval, coords = c("X", "Y"), crs = st_crs(x))
  return(sfInterval)
}

listIntervals <- lapply(listPts, GetInterval)
allIntervals <- do.call(rbind, listIntervals)

leaflet() %>% 
  addProviderTiles(provider = "Stamen.TonerLite") %>% 
  addCircles(data = st_transform(allIntervals, crs = 4326), 
             radius = 0.1 * sqrt(allIntervals$INTERVAL / pi),
             stroke = FALSE, fill = TRUE, fillColor = "firebrick", fillOpacity = 0.5)



# make rectangular grid and intersect ----

geoGrid <- st_make_grid(x = bboxDormoy, cellsize = 120, crs = 2154, what = "polygons")
oneGrid <- st_sf(IDGRID = seq(1, length(geoGrid), 1),
                 geometry = geoGrid)

interGrid <- st_intersects(allIntervals, oneGrid)
idIn <- sapply(interGrid, length)
allIntervals$IDGRID <- sapply(interGrid, function(x) ifelse(length(x) > 0, x, 0))

aggrTime <- allIntervals %>%
  filter(IDGRID > 0) %>% 
  st_set_geometry(NULL) %>% 
  group_by(IDGRID) %>% 
  summarise(SUMTIME = sum(INTERVAL))

oneGrid <- oneGrid %>% 
  left_join(aggrTime, by = "IDGRID") %>% 
  mutate(SUMTIME = ifelse(is.na(SUMTIME), 0, SUMTIME),
         PCTTIME = SUMTIME / sum(SUMTIME))


# map gridded time ----

gradOrange <- colorRampPalette(c("grey90", "orangered4"))(10)
oneGrid$COL <- cut(oneGrid$PCTTIME, breaks = seq(0, 0.2, 0.02), labels = 1:10, include.lowest = TRUE, right = FALSE)
leaflet() %>% 
  addProviderTiles(provider = "Stamen.TonerLite") %>% 
  addPolygons(data = st_transform(oneGrid, crs = 4326),
              stroke = NULL, fill = TRUE, fillOpacity = 0.8,
              fillColor = gradOrange[oneGrid$COL]) %>% 
  addCircles(data = st_transform(allIntervals, crs = 4326), 
             radius = 0.1 * sqrt(allIntervals$INTERVAL / pi),
             stroke = FALSE, fill = TRUE, fillColor = "grey30", fillOpacity = 0.5)
