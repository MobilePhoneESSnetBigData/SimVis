library(data.table)
library(ggplot2)
library(scales)
library(rgeos)
library(png)
library(XML)
library(ggforce)
library(gridGraphics)
library(ggpubr)
library(raster)
#path <- 'C:/simulator'
path <- 'D:/r-projects/SimVis'

signalQuality <- fread(file.path(path, 'SignalQuality_Vodafone.csv'), sep =",", header = TRUE, stringsAsFactors = FALSE)
nTiles <- dim(signalQuality)[2] - 1
setnames(signalQuality, c('antenna', 0:(nTiles - 1)))
signalQuality.mt <- melt(signalQuality, id.vars = 'antenna', variable.name = 'tile', variable.factor = FALSE)
signalQuality <- dcast(signalQuality.mt, formula = tile ~ antenna, value.var = 'value')[order(as.integer(tile))]

map <- readWKT(readLines(file.path(path, "map.wkt")))
map.polygon <- map@polygons[[1]]@Polygons[[1]]@coords

gridParam <- fread(file.path(path, 'grid.csv'), sep = ',', header = TRUE, stringsAsFactors = FALSE)
tileDimX <- gridParam[['X Tile Dim']]
tileDimY <- gridParam[['Y Tile Dim']]
noTilesX <- gridParam[['No Tiles X']]
noTilesY <- gridParam[['No Tiles Y']]
originX <- gridParam[['Origin X']]
originY <- gridParam[['Origin Y']]

antennaPosition <- fread(file.path(path, 'antennas.csv'), sep = ',', header = TRUE, stringsAsFactors = FALSE)[order(as.integer(get('Antenna ID')))]

antenna_img <-  readPNG(file.path(path, "omnidirectional-antenna-tiny.png"))
antenna.grob <- rasterGrob(antenna_img, interpolate = FALSE)

qualityRaster <- raster(nrow = noTilesX, ncol = noTilesY, 
                        xmn = originX, xmx = originX + noTilesX * tileDimX,
                        ymn = originY, ymx = originY + noTilesY * tileDimY)
values(qualityRaster) <- signalQuality[['2']]
#plot(qualityRaster)

qualityRaster.df <- as.data.frame(xyFromCell(qualityRaster, 1:ncell(qualityRaster)))
qualityRaster.df <- cbind(qualityRaster.df, quality = getValues(qualityRaster))
ant <- antennaPosition[1]
ggplot() + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin=unit(c(0, 0, 0, 0),"cm"),
        legend.position = 'top') +
  geom_polygon(aes(x = map.polygon[, 1], y = map.polygon[, 2]), 
               fill = "#7D7D7D", alpha = 0.2) + 
#  annotation_raster(antenna_img, 
#                    xmin = max(ant[['x']] - 150, 0), xmax = min(ant[['x']] + 150, 10000), 
#                    ymin = max(ant[['y']] - 150, 0), ymax = min(ant[['y']] + 150, 10000)) + 
  geom_point(data = data.frame(x = 1250, y = 6000), mapping = aes(x=x, y=y), col = 'red', size = 2) +
  geom_point(data = data.frame(x = 1250, y = 4250), mapping = aes(x=x, y=y), col = 'blue', size = 2) +
  geom_raster(data= qualityRaster.df, aes(x=x, y=y, fill=quality), alpha = 0.4) + 
  scale_fill_gradient2(low=muted('green'), mid='white', high=muted('red')) +
  coord_fixed()
