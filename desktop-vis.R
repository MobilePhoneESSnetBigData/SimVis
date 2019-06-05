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
p <- p + geom_point(shape = 8, size = 6, data = df2, aes(x = antennas[,3], y = antennas[,4]), colour = "#CC0000")
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
# t is time
for(t in 1:200) {
  min[t] = min(prob[t,3:102])
  min[t] = min[t] - 0.01*min[t]
  max[t] = max(prob[t,3:102])
  limits[[t]] <- c(min[t])
  dif[t] = max[t] - min[t]
  for(j in 1:75) {
    x[j] <- min[t] + j * dif[t]/75
    limits[[t]] <- c(limits[[t]],x[j])
  }
}

m<-list()
pf<-list()
for(t in 1:200) {
  m[[t]] <- data.frame()
  pf[[t]]<-cut(as.numeric(prob[t,3:102]), limits[[t]], labels = c(1:75), right = TRUE)
  for(j in 1:10) {
    index = (j-1)*10 + 1:10
    r <- pf[[t]][index]
    m[[t]] <- rbind(m[[t]],r)    
  }
}


m


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

p <- ggplot() + geom_polygon(aes(x = datapoly[,1], y = datapoly[,2], fill = "gray") , alpha = 0.5)
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks=NULL) 
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks=NULL)
p <- p + guides(size=FALSE)+theme_bw()


for(t in 75:75) {
  #print(cat("timpul ", t))
  for(i in 0:99) {
    nr <- floor(i /  grid$No.Tiles.X)
    nc <- floor(i - nr * grid$No.Tiles.X)
    if(m[[t]][nr+1,nc+1] == 75) {
      #print(cat(nr, " , ", nc))
      d <- fortify(datatile[[i+1]])
      p <- p + geom_polygon(aes_string(x = d$x, y = d$y))
    }
  }
  #d <- fortify(datatile[[t]])
  #p <- p + geom_polygon(aes_string(x = d$x, y = d$y))
}



#p <- p + geom_point(data = persons, aes(x = persons[,3], y = persons[,4]) ) 
#p <- p + transition_states(persons[,1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.005, alpha = FALSE)
#options(gganimate.dev_args = list(width = 600, height = 600))
#animate(p, nframes = 400, rewind = FALSE)




#######in lucru################
ssteep = 0.2
smid = -92.5
power = 10
gamma = 3.5
distance = 6000
S0 = 30 + 10 * log10(power)
Sd = 10 * gamma * log10(distance)
S = S0 - Sd
result = 1.0 / (1 + exp(-ssteep * (S - smid) ) )

