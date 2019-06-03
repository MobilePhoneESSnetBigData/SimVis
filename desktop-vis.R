library(ggplot2)
library(gganimate)
library(rgeos)

setwd("D:/r-projects/SimVis")

#read map
con <- file("map.wkt", open = "r")
lines <- readLines(con)
map <- readWKT(lines)
datapoly = map@polygons[[1]]@Polygons[[1]]@coords
datapoly <- as.data.frame(datapoly)


#read persons
persons <- read.csv("persons.csv", stringsAsFactors = FALSE, header = FALSE)

#read antennas
antennas <- read.csv("antennas.csv", stringsAsFactors = FALSE, header = FALSE)

#read grid
grid<-read.csv(file="grid.csv", stringsAsFactors = FALSE, header = TRUE)
gridpointsx = seq(from = 0, to = grid$No.Tiles.X*grid$X.Tile.Dim, by = grid$X.Tile.Dim)
gridpointsy = seq(from = 0, to = grid$No.Tiles.Y*grid$Y.Tile.Dim, by = grid$Y.Tile.Dim)

#plot persons and antennas
p <- ggplot( datapoly, aes(x = datapoly[,1], y = datapoly[,2])) + geom_polygon(aes(fill = "gray") , alpha = 0.5)
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks=NULL) 
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks=NULL)
p <- p + guides(size=FALSE)+theme_bw()
p <- p + geom_point(data = persons, aes(x = persons[,3], y = persons[,4]) ) 
p <- p + transition_states(persons[,1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.005, alpha = FALSE)
animate(p, nframes = 880, rewind = FALSE)


#read prob
prob <- read.csv(file="prob.csv", stringsAsFactors = FALSE, header = FALSE)
#normalize prob
for(i in 1:nrow(prob)) {
  s<- sum(prob[i, 3:ncol(prob)])
  prob[i, 3:ncol(prob)] <-  prob[i, 3:ncol(prob)]/s
}
