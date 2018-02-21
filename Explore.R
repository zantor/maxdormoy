
library(jsonlite)
library(lubridate)
library(sf)
library(leaflet)

options(scipen = 100000)

allFiles <- list.files(path = "DATA/Records-2017-12-16/")
allNames <- gsub(x = allFiles, pattern = "_|-| |default|.json", replacement = "")
allNamesCorr <- gsub(x = allNames, pattern = "[[:digit:]]", replacement = "")

oneJson <- fromJSON(txt = "DATA/Records-2017-12-16/default_Farrah_20171116-103915.json", 
                    simplifyDataFrame = TRUE,
                    flatten = TRUE)
dataJson <- oneJson$data

sfPts <- st_as_sf(dataJson, agr = "constant", 
                  coords = c("newValue.y", "newValue.x"), 
                  crs = 4326)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = sfPts)


# compute time interval ----

dataJson$CONCAT <- paste(dataJson$newValue.x, dataJson$newValue.y, sep = "_")
dataJson$DUPLI <- !duplicated(dataJson$CONCAT)
sfPts <- dataJson[dataJson$DUPLI, ] %>% 
  st_as_sf(agr = "constant", coords = c("newValue.y", "newValue.x"), crs = 4326) %>% 
  st_transform(crs = 2154)

x = sfPts
GetInterval <- function(x){
  initCoords <- st_coordinates(x)
  valTime <- as.vector(x$timestamp)
  difTime <- valTime[2:length(valTime)] - valTime[1:length(valTime)-1]
  xCoords <- mapply(mean, initCoords[1:nrow(initCoords)-1, 1], initCoords[2:nrow(initCoords), 1])
  yCoords <- mapply(mean, initCoords[1:nrow(initCoords)-1, 2], initCoords[2:nrow(initCoords), 2])
  tabInterval <- data.frame(X = xCoords, Y = yCoords, INTERVAL = difTime)
  return(tabInterval)
}

bibi <- GetInterval(x = sfPts)
plot(bibi$X, bibi$Y, cex = 0.00001 * bibi$INTERVAL, pch = 20)
plot(sfPts$geometry)
