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
p <- p + geom_point(shape = 8, size = 6, data = antennas, aes(x = antennas[,3], y = antennas[,4]), colour = "#CC0000")
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks=NULL) 
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks=NULL)
p <- p + guides(size=FALSE)+theme_bw()
p <- p + geom_point(data = persons, aes(x = persons[,3], y = persons[,4]) ) 
p <- p + transition_states(persons[,1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.005, alpha = FALSE)
options(gganimate.dev_args = list(width = 600, height = 600))
animate(p, nframes = 400, rewind = FALSE)


#read prob file
prob <- read.csv(file="prob.csv", stringsAsFactors = FALSE, header = FALSE)
#normalize prob
for(i in 1:nrow(prob)) {
  s<- sum(prob[i, 3:ncol(prob)])
  prob[i, 3:ncol(prob)] <-  prob[i, 3:ncol(prob)]/s
}

#select  the mobile with Id=0
prob<-prob[prob[,2]==0,]
limits<-list()
min <- c()
max <- c()
dif <- c()
x <- c()
# t is time
NLevels <- 35
for(t in 1:200) {
  min[t] = min(prob[t,3:102])
  min[t] = min[t] - 0.01*min[t]
  max[t] = max(prob[t,3:102])
  limits[[t]] <- c(min[t])
  dif[t] = max[t] - min[t]
  for(j in 1:NLevels) {
    x[j] <- min[t] + j * dif[t]/NLevels
    limits[[t]] <- c(limits[[t]],x[j])
  }
}

m<-list()
pf<-list()
for(t in 1:200) {
  m[[t]] <- data.frame()
  pf[[t]]<-cut(as.numeric(prob[t,3:102]), limits[[t]], labels = c(1:NLevels), right = TRUE)
  for(j in 1:10) {
    index = (j-1)*10 + 1:10
    r <- pf[[t]][index]
    m[[t]] <- rbind(m[[t]],r)    
  }
}

for(i in 1:200) {
  m[[i]][is.na(m[[i]])]<-0
}



#### desenez probabilitatile######

## 1. imi trebuie conturul fiecarei tile
### datatile[i] contine conturul tilei i
datatile<-list()
x<-list()
y<-list()
for(i in 0:99) {
  nr <- floor(i /  grid$No.Tiles.X)
  nc <- floor(i - nr * grid$No.Tiles.X)
  xs <- nc * grid$X.Tile.Dim
  xe <- xs + grid$X.Tile.Dim
  ys <- nr * grid$Y.Tile.Dim
  ye <- ys + grid$Y.Tile.Dim
  x[[i+1]]<-c(xs,xe,xe,xs)
  y[[i+1]]<-c(ys,ys,ye,ye)
  datatile[[i+1]]<-data.frame(x = x[[i+1]], y = y[[i+1]])
}

# t = 1

p <- ggplot() + geom_polygon(aes(x = datapoly[,1], y = datapoly[,2]), fill = "#7D7D7D" , alpha = 0.5)
p <- p + geom_point(shape = 8, size = 6, data = antennas, aes(x = antennas[,3], y = antennas[,4]), colour = "#CC0000")
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks=NULL) 
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks=NULL)
p <- p + guides(size=FALSE)+theme_bw()

persons<-persons[( persons[,2]==2),]

for(t in 1:200) {
  r <- p
  for(i in 0:99) {
    nr <- floor(i /  grid$No.Tiles.X)
    nc <- floor(i - nr * grid$No.Tiles.X)
    if(m[[t]][nr+1,nc+1] == NLevels) {
      d <- fortify(datatile[[i+1]])
      r <- r + geom_polygon(aes_string(x = d$x, y = d$y), alpha = 0.7)
      r <- r + geom_point(data = persons[persons[,1] == t,], aes_string(x = persons[persons[,1] == t,][1,3], y = persons[persons[,1] == t,][1,4]) ) 
      
    }
    
  }
  print(t)
  print(r)
  Sys.sleep(0.1)
}

persons<-persons[( persons[,2]==2),]

ploturi_pers<-data.frame(t = NULL, x = NULL, y = NULL)

for(t in 1:200) {
  ploturi_pers[t,1] <- t
  ploturi_pers[t, 2] <- persons[persons[,1] == t-1,][1,3]
  ploturi_pers[t, 3] <- persons[persons[,1] == t-1,][1,4]
}

ploturi_poligoane<-data.frame(t = c(), x = c(), y = c())
for(t in 1:200) {
  tmp <-data.frame(t = c(), x = c(), y = c())
  for(i in 0:99) {
    nr <- floor(i /  grid$No.Tiles.X)
    nc <- floor(i - nr * grid$No.Tiles.X)
    if(m[[t]][nr+1,nc+1] == NLevels) {
      d <- fortify(datatile[[i+1]])
      row1 <- c(t, d$x[1], d$y[1])
      row2 <- c(t, d$x[2], d$y[2])
      row3 <- c(t, d$x[3], d$y[3])
      row4 <- c(t, d$x[4], d$y[4])
      tmp <-rbind(tmp, row1)
      tmp <-rbind(tmp, row2)
      tmp <-rbind(tmp, row3)
      tmp <-rbind(tmp, row4)
    }
  }
  #cpoints<-chull(tmp[,2:3])
  #cpoints<-c(cpoints, cpoints[1])
  colnames(tmp)<-c("t", "x", "y")
  ploturi_poligoane <- rbind(ploturi_poligoane, tmp)
}


p <- ggplot() + geom_polygon(aes(x = datapoly[,1], y = datapoly[,2]), fill = "#7D7D7D" , alpha = 0.5)
p <- p + geom_point(shape = 8, size = 6, data = antennas, aes(x = antennas[,3], y = antennas[,4]), colour = "#CC0000")
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks=NULL) 
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks=NULL)
p <- p + guides(size=FALSE)+theme_bw()

p <- p + geom_point(data = ploturi_pers, aes(x = ploturi_pers[,2], y = ploturi_pers[,3]) ) 
p <- p + geom_path(data = ploturi_poligoane, aes(x = ploturi_poligoane[,2], y = ploturi_poligoane[,3]), alpha = 0.5)

p <- p + transition_states(ploturi_poligoane[,1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.005)
#p <- p + transition_states(ploturi_pers[,1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.005, alpha = FALSE)


options(gganimate.dev_args = list(width = 400, height = 400))
animate(p, nframes = 400, rewind = FALSE)



#######in lucru################
ssteep = 0.2
smid = -92.5
power = 10
gamma = 3.87
distance = sqrt((7000-9500)^2 + (2500-9500)^2)
S0 = 30 + 10 * log10(power)
Sd = 10 * gamma * log10(distance)
S = S0 - Sd
result = 1.0 / (1 + exp(-ssteep * (S - smid) ) )

