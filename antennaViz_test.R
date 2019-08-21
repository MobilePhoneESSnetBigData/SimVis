library(XML)
library(rgeos)
library(ggplot2)
library(ggforce)
library(png)
library(gridGraphics)
library(ggpubr)

path <- 'D:/r-projects/SimVis'


source(file.path(path, 'plotAntennas.R'))
antenna_img <-  readPNG(file.path(path, "omnidirectional-antenna-tiny.png"))

# City District in Madrid (Chamartín)
map <- readWKT(readLines(file.path(path, "mapMadrid.wkt")))
map.polygon <- map@polygons[[1]]@Polygons[[1]]@coords

plotAntennas(map.polygon, file.path(path, 'antennasMadrid.xml'), antenna_img)
plotAntennas(map.polygon, file.path(path, 'antennasMadrid.xml'), antenna_img, cover = TRUE)

# Silly example
map <- readWKT(readLines(file.path(path, "map.wkt")))
map.polygon <- map@polygons[[1]]@Polygons[[1]]@coords

plotAntennas(map.polygon, file.path(path, 'newantennas.xml'), antenna_img)
plotAntennas(map.polygon, file.path(path, 'newantennas.xml'), antenna_img, cover = TRUE)
