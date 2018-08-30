###############################################
##### Explore GPS tracks : 1st experiment #####
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
lineRoad <- st_read(dsn = "DATA/basemap/troncon_dormoy.shp", stringsAsFactors = FALSE, crs = 2154)

# library(mapview)
# library(mapedit)
# boxSelection <- editMap(mapview())
# boxSelection <- boxSelection$finished
# saveRDS(boxSelection, file = "bboxdormoy.Rds")

bboxDormoy <- readRDS(file = "DATA/basemap/bboxdormoy.Rds") %>% st_transform(crs = 2154)


# extract one JSON ----

oneJson <- fromJSON(txt = "DATA/Records-2017-12-16/default_Farrah_20171116-103915.json", 
                    simplifyDataFrame = TRUE,
                    flatten = TRUE)
dataJson <- oneJson$data


# clean records and create spatial object ----

dataJson$CONCAT <- paste(dataJson$newValue.x, dataJson$newValue.y, sep = "_")
dataJson$DUPLI <- !duplicated(dataJson$CONCAT)

onePts <- dataJson[dataJson$DUPLI, ] %>% 
  st_as_sf(agr = "constant", coords = c("newValue.y", "newValue.x"), crs = 4326) %>% 
  st_transform(crs = 2154)


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

oneInterval <- GetInterval(x = onePts)

leaflet() %>% 
  addProviderTiles(provider = "Stamen.TonerLite") %>% 
  addCircles(data = st_transform(oneInterval, crs = 4326), 
             radius = 0.1 * sqrt(oneInterval$INTERVAL / pi),
             stroke = FALSE, fill = TRUE, fillColor = "firebrick", fillOpacity = 0.5)


# make rectangular grid and intersect ----

geoGrid <- st_make_grid(x = bboxDormoy, cellsize = 120, crs = 2154, what = "polygons")
oneGrid <- st_sf(IDGRID = seq(1, length(geoGrid), 1),
                 geometry = geoGrid)

leaflet() %>% 
  addProviderTiles(provider = "Stamen.TonerLite") %>% 
  addPolygons(data = st_transform(oneGrid, crs = 4326))

interGrid <- st_intersects(oneInterval, oneGrid)
oneInterval$IDGRID <- unlist(interGrid)
aggrTime <- oneInterval %>% 
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
  addCircles(data = st_transform(oneInterval, crs = 4326), 
             radius = 0.1 * sqrt(oneInterval$INTERVAL / pi),
             stroke = FALSE, fill = TRUE, fillColor = "grey30", fillOpacity = 0.5)


# create street network ----

class(lineRoad)
centroLine <- as.data.frame(st_coordinates(x = lineRoad)) %>% 
  mutate(XROUND = formatC(X, digits = 3, drop0trailing = FALSE, format = "f"),
         YROUND = formatC(Y, digits = 3, drop0trailing = FALSE, format = "f"),
         KEY = paste(XROUND, YROUND, sep = "_"))



centroLine$DUP1 <- !duplicated(centroLine$L1)
centroLine$DUP2 <- rev(!duplicated(rev(centroLine$L1)))
centroNodes <- centroLine %>% 
  filter(DUP1 == TRUE | DUP2 == TRUE) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 2154)

plot(lineRoad$geometry)
plot(centroNodes$geometry, add = TRUE)

edgeList <- centroNodes %>% st_set_geometry(NULL)
edgeListOri <- edgeList$KEY[seq(1, nrow(edgeList)-1, by = 2)]
edgeListDes <- edgeList$KEY[seq(2, nrow(edgeList), by = 2)]
oriDes <- cbind(edgeListOri, edgeListDes) %>% as_data_frame()



netRoad <- graph.data.frame(d = oriDes, directed = FALSE)
summary(netRoad)
V(netRoad)$X <- as.numeric(substr(V(netRoad)$name, 1, 10))
V(netRoad)$Y <- as.numeric(substr(V(netRoad)$name, 12, 22))

plot(netRoad,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = "black",
     edge.color = "firebrick",
     edge.width = 2,
     layout = cbind(V(netRoad)$X, V(netRoad)$Y))
