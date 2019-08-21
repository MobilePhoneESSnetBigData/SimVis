plotAntennas <- function(map_matrix, antennasFileName, antenna_img, cover = FALSE, tileDim = c(100, 100), noTiles = c(500, 500)){
  
  rmax <- function(Smin, Power, gamma){
    
    Smin <- as.numeric(Smin)
    Power <- as.numeric(Power)
    gamma <- as.numeric(gamma)
    out <- 10^{(3 - 0.1 * Smin) / gamma} * Power^{10 / gamma}
    return(out)
  }
  
  antenna.grob <- rasterGrob(antenna_img, interpolate = FALSE)
  antennas.conf <- xmlToList(antennasFileName)
  
  
  coverArea <- Reduce(
    rbind, 
    lapply(antennas.conf, function(antenna){as.numeric(c(antenna$x, antenna$y))}))
  dimnames(coverArea) <- list(paste0('antenna', 1:dim(coverArea)[1]), c('x', 'y'))
  
  antennaParam <- Reduce(
    rbind,
    lapply(antennas.conf, function(antenna){as.numeric(c(antenna$Smin, antenna$power, antenna$attenuationfactor))}))
  dimnames(antennaParam) <- list(paste0('antenna', 1:dim(coverArea)[1]), c('Smin', 'Power', 'gamma'))
  rMax <- apply(antennaParam, 1, function(x){do.call(rmax, as.list(x))})
  coverArea <- as.data.frame(coverArea)
  coverArea$r <- rMax

  tileDimX <- tileDim[1]
  tileDimY <- tileDim[2]
  noTilesX <- noTiles[1]
  noTilesY <- noTiles[2]
  
  gridpointsx <- seq(from = min(map_matrix[, 'x']), to = min(min(map_matrix[, 'x']) + noTilesX * tileDimX, max(map_matrix[, 'x'])), by = tileDimX)
  gridpointsy <- seq(from = min(map_matrix[, 'y']), to = min(min(map_matrix[, 'y']) + noTilesY * tileDimY, max(map_matrix[, 'y'])), by = tileDimY)
  
  
  p <- ggplot() + 
    theme(panel.background = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_line(colour = "white"),
          plot.margin=unit(c(0, 0, 0, 0),"cm"),
          legend.position = 'none') +
    geom_polygon(aes(x = map_matrix[, 1], y = map_matrix[, 2]), 
                 fill = "#7D7D7D", alpha = 0.2) + 
    coord_fixed() +
    mapply(function(xx, yy) annotation_raster(antenna_img, xmin = xx - 75, xmax = xx + 75, ymin = yy - 75, ymax = yy + 75),
             xx <- coverArea[, 'x'] , yy <- coverArea[, 'y']) + 
    scale_x_continuous(breaks = gridpointsx, minor_breaks = NULL) +
    scale_y_continuous(breaks = gridpointsy, minor_breaks = NULL) +
    grids(size = 0.5)

  if (cover) {  
    p <- p  + 
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = 'red', color = 'red'), 
                data = coverArea, alpha = 0.05)
  }
    
  p
}
