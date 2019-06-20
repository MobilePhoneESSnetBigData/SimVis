library(ggplot2)
library(gganimate)
library(rgeos)
library(png)

setwd("D:/r-projects/SimVis")

#read map
con <- file("map.wkt", open = "r")
lines <- readLines(con)
map <- readWKT(lines)
datapoly = map@polygons[[1]]@Polygons[[1]]@coords
datapoly <- as.data.frame(datapoly)

#read antenna symbol
antenna_img <-  readPNG("omnidirectional-antenna-tiny.png")

#read persons
persons <- read.csv("persons.csv", stringsAsFactors = FALSE, header = FALSE)

#read antennas

antennas <- read.csv("antennas.csv",stringsAsFactors = FALSE, header = FALSE)

#read grid
grid <- read.csv(file = "grid.csv", stringsAsFactors = FALSE, header = TRUE)
gridpointsx = seq(from = 0, to = grid$No.Tiles.X * grid$X.Tile.Dim, by = grid$X.Tile.Dim)
gridpointsy = seq(from = 0, to = grid$No.Tiles.Y * grid$Y.Tile.Dim, by = grid$Y.Tile.Dim)

#plot persons and antennas
p <- ggplot() + geom_polygon(aes(x = datapoly[, 1], y = datapoly[, 2]), fill = "#7D7D7D", alpha = 0.5)
p <- p + mapply(function(xx, yy) annotation_raster(antenna_img, xmin = xx - 200, xmax = xx + 200, ymin = yy - 200, ymax = yy + 200),xx <- antennas[, 3] ,yy <- antennas[, 4] )

p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks = NULL)
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks = NULL)
p <- p + guides(size = FALSE) + theme_bw()
p <- p + xlab(label = "Longitude") + ylab("Latitude")
p <- p + geom_point(data = persons,aes(x = persons [, 3], y = persons[, 4], shape = ifelse(is.na(persons[, 5]), "NoSIM", "SIM"), size = ifelse(is.na(persons[, 5]), "NoSIM", "SIM") )) + scale_shape_manual(name = "", values = c(NoSIM = 2, SIM = 4)) + scale_size_manual(name = "", values = c(NoSIM = 2, SIM = 4))
p <- p + transition_states(persons[, 1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.025, alpha = FALSE)


options(gganimate.dev_args = list(width = 600, height = 600))
movie <- animate(p, renderer = ffmpeg_renderer(), nframes = 400, rewind = FALSE, duration = 40)


anim_save(filename = "simulation-20iunie.mpg", animation = movie)


#read prob file
prob <- read.csv(file = "prob.csv",stringsAsFactors = FALSE, header = FALSE)
#normalize prob
for (i in 1:nrow(prob)) {
  s <- sum(prob[i, 3:ncol(prob)])
  if(s > 0)
    prob[i, 3:ncol(prob)] <-  prob[i, 3:ncol(prob)] / s
}

#select  the mobile with Id=0
prob <- prob[prob[, 2] == 1, ]
TMAX <- max(prob[,1]) + 1 #t starts from 0 in prob.csv
NLevels <- 75
m <- list()
pf <- list()
for (t in 1:TMAX) {
  m[[t]] <- data.frame()

    ncols <- 3:((3+grid$No.Tiles.X*grid$No.Tiles.Y) - 1)
  if(sum(prob[t, ncols]) > 0)
    pf[[t]] <- cut(as.numeric(prob[t, ncols]), breaks = NLevels,labels = c(1:NLevels),right = TRUE, include.lowest = TRUE)
  else
    pf[[t]] <- rep(1, grid$No.Tiles.X*grid$No.Tiles.Y)

####aici corectez
  for (j in 1:grid$No.Tiles.Y) {
    index = (j - 1) * grid$No.Tiles.X + 1:grid$No.Tiles.Y
    r <- pf[[t]][index]
    m[[t]] <- rbind(m[[t]], r)
  }
}

for (i in 1:TMAX) {
  m[[i]][is.na(m[[i]])] <- 0
}

#### desenez probabilitatile######

## 1. imi trebuie conturul fiecarei tile
### datatile[i] contine conturul tilei i

datatile <- list()
x <- list()
y <- list()
noTiles <- grid$No.Tiles.X * grid$No.Tiles.Y
for (i in 0:(noTiles - 1) ){
  nr <- floor(i /  grid$No.Tiles.X)
  nc <- floor(i - nr * grid$No.Tiles.X)
  xs <- nc * grid$X.Tile.Dim
  xe <- xs + grid$X.Tile.Dim
  ys <- nr * grid$Y.Tile.Dim
  ye <- ys + grid$Y.Tile.Dim
  x[[i + 1]] <- c(xs, xe, xe, xs)
  y[[i + 1]] <- c(ys, ys, ye, ye)
  datatile[[i + 1]] <- data.frame(x = x[[i + 1]], y = y[[i + 1]])
}

#Selectez persoanele cu telefoane mobile
persons <- persons[(persons[, 2] == 3), ]

ploturi_pers <- data.frame(t = numeric(), x = numeric(), y = numeric())

for (t in 1:TMAX) {
  ploturi_pers[t, 1] <- t
  ploturi_pers[t, 2] <- persons[persons[, 1] == t - 1, ][1, 3]
  ploturi_pers[t, 3] <- persons[persons[, 1] == t - 1, ][1, 4]
}

ploturi_poligoane <- data.frame(t = numeric(), x = numeric(), y = numeric())
for (t in 1:TMAX) {
  poligons <- list()
  for (i in 0:(noTiles-1)) {
    nr <- floor(i /  grid$No.Tiles.X)
    nc <- floor(i - nr * grid$No.Tiles.X)
    if (m[[t]][nr + 1, nc + 1] >= NLevels-2) {
      d <- fortify(datatile[[i + 1]])
      polstr <- "POLYGON(("
      polstr <- paste(polstr, d$x[1], " ", d$y[1], ",")
      polstr <- paste(polstr, d$x[2], " ", d$y[2], ",")
      polstr <- paste(polstr, d$x[3], " ", d$y[3], ",")
      polstr <- paste(polstr, d$x[4], " ", d$y[4], ",")
      polstr <- paste(polstr, d$x[1], " ", d$y[1], "))")
      sppol <- readWKT(polstr)
      poligons <- c(poligons, sppol)
    }
  }
  uniune <- poligons[[1]]
  if (length(poligons) > 1) {
    for (j in 2:length(poligons)) {
      uniune <- gUnion(uniune, poligons[[j]])
    }
  }
  tmp <- data.frame(t = numeric(), x = numeric(), y = numeric())

  for (j in 1:nrow(uniune@polygons[[1]]@Polygons[[1]]@coords)) {
    tmp <- rbind(tmp, c(t,uniune@polygons[[1]]@Polygons[[1]]@coords[j, 1], uniune@polygons[[1]]@Polygons[[1]]@coords[j, 2]))
    colnames(tmp) <- c("t", "x", "y")
  }
  ploturi_poligoane <- rbind(ploturi_poligoane, tmp)
}

df <- merge(ploturi_pers,ploturi_poligoane,by.x = "t", by.y = "t")

remove(p)
p <- ggplot() + geom_polygon(aes(x = datapoly[, 1], y = datapoly[, 2]), fill = "#7D7D7D", alpha = 0.5)
p <- p + mapply(  function(xx, yy) annotation_raster(antenna_img, xmin = xx - 200, xmax = xx + 200, ymin = yy - 200, ymax = yy + 200), xx <- antennas[, 3], yy <- antennas[, 4])
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks = NULL)
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks = NULL)
p <- p + guides(size = FALSE) + theme_bw()
p <- p + xlab(label = "Longitude") + ylab("Latitude")
p <- p + geom_point(data = df, aes(x = df[, 2], y = df[, 3]), size = 4, shape = 4)
p <- p + geom_polygon(data = df, aes(x = df[, 4], y = df[, 5]), alpha = 0.05)
p <- p + transition_states(df[, 1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.025, alpha = FALSE)

options(gganimate.dev_args = list(width = 600, height = 600))
movie2 <- animate( p, nframes = 400, renderer = ffmpeg_renderer(), rewind = FALSE, duration = 40)

anim_save(filename = "simulation-network-prior-20iunie.mpg", animation = movie2)



#######in lucru################
ssteep = 0.2
smid = -92.5
power = 10
gamma = 3.87
distance = sqrt((4000-3500) ^ 2 + (7000-9500) ^ 2)
S0 = 30 + 10 * log10(power)
Sd = 10 * gamma * log10(distance)
S = S0 - Sd
result = 1.0 / (1 + exp(-ssteep * (S - smid)))


street1 <- read.csv("street1.csv",stringsAsFactors = FALSE, header = FALSE)
street2 <- read.csv("street2.csv",stringsAsFactors = FALSE, header = FALSE)
person_move <- read.csv("person_move.csv",stringsAsFactors = FALSE, header = FALSE)

p<-ggplot() + geom_path(aes(x = street1$V1, y = street1$V2))
p <- p + geom_path(aes(x = street2$V1, y = street2$V2))
p <- p + geom_point(data = person_move,aes(x = person_move[, 2], y = person_move[, 3]))
p <- p + transition_states(person_move[, 1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.025, alpha = FALSE)
options(gganimate.dev_args = list(width = 600, height = 600))
movie2 <- animate( p, nframes = 700, renderer = ffmpeg_renderer(), rewind = FALSE, duration = 40)

