library(ggplot2)
library(gganimate)
library(rgeos)
library(png)

#setwd("D:/r-projects/SimVis")

#read map
con <- file("map.wkt", open = "r")
lines <- readLines(con)
map <- readWKT(lines)
datapoly = map@polygons[[1]]@Polygons[[1]]@coords
datapoly <- as.data.frame(datapoly)

#read antenna symbol
antenna_img <-  readPNG("antenna.png")

#read persons
persons <- read.csv("persons.csv",
             stringsAsFactors = FALSE,
             header = FALSE)

#read antennas
antennas <- read.csv("antennas.csv",
             stringsAsFactors = FALSE,
             header = FALSE)

#read grid
grid <- read.csv(file = "grid.csv",
             stringsAsFactors = FALSE,
             header = TRUE)
gridpointsx = seq(
    from = 0,
    to = grid$No.Tiles.X * grid$X.Tile.Dim,
    by = grid$X.Tile.Dim
)
gridpointsy = seq(
    from = 0,
    to = grid$No.Tiles.Y * grid$Y.Tile.Dim,
    by = grid$Y.Tile.Dim
)

#plot persons and antennas
p <- ggplot() + geom_polygon(aes(x = datapoly[, 1], y = datapoly[, 2]),
                            fill = "#7D7D7D" ,
                            alpha = 0.5)
p <- p + geom_point(
        shape = 8,
        size = 6,
        data = antennas,
        aes(x = antennas[, 3], y = antennas[, 4]),
        colour = "#CC0000") + annotation_raster(antenna_img, ymin = antennas[, 4]),ymax= antennas[, 4])+10,xmin = antennas[, 3],xmax = antennas[, 3]+10)
p <- p + scale_y_continuous(breaks = gridpointsy, minor_breaks = NULL)
p <- p + scale_x_continuous(breaks = gridpointsx, minor_breaks = NULL)
p <- p + guides(size = FALSE) + theme_bw()
p <- p + xlab(label = "Longitude") + ylab("Latitude")
# for ( i in 1:nrow(antennas)) {
#     p <- p + annotation_raster(antenna_img, ymin = antennas[i, 4],ymax = antennas[i, 4]+500,xmin = antennas[i, 3],xmax = antennas[i, 3]+500) +geom_point()
# }
p <-
    p + geom_point(data = persons,
                   aes(
                       x = persons [, 3],
                       y = persons[, 4],
                       shape = ifelse(is.na(persons[, 5]), "NoSIM", "SIM"),
                       size = ifelse(is.na(persons[, 5]), "NoSIM", "SIM")
                   )) + scale_shape_manual(name = "", values = c(NoSIM = 2, SIM = 4)) + scale_size_manual(name = "", values = c(NoSIM = 2, SIM = 4))
p <- p + transition_states(persons[, 1],
                          transition_length = 1,
                          state_length = 1) + shadow_wake(wake_length = 0.025, alpha = FALSE)
options(gganimate.dev_args = list(width = 600, height = 600))
movie <- animate(p,
            renderer = av_renderer(),
            nframes = 400,
            rewind = FALSE)

anim_save(filename = "simulation1.mp4", animation = movie)




#read prob file
prob <-
    read.csv(file = "prob.csv",
             stringsAsFactors = FALSE,
             header = FALSE)
#normalize prob
for (i in 1:nrow(prob)) {
    s <- sum(prob[i, 3:ncol(prob)])
    prob[i, 3:ncol(prob)] <-  prob[i, 3:ncol(prob)] / s
}

#select  the mobile with Id=0
prob <- prob[prob[, 2] == 0, ]
limits <- list()
min <- c()
max <- c()
dif <- c()
x <- c()
# t is time
NLevels <- 35
for (t in 1:200) {
    min[t] = min(prob[t, 3:102])
    min[t] = min[t] - 0.01 * min[t]
    max[t] = max(prob[t, 3:102])
    limits[[t]] <- c(min[t])
    dif[t] = max[t] - min[t]
    for (j in 1:NLevels) {
        x[j] <- min[t] + j * dif[t] / NLevels
        limits[[t]] <- c(limits[[t]], x[j])
    }
}

m <- list()
pf <- list()
for (t in 1:200) {
    m[[t]] <- data.frame()
    pf[[t]] <-
        cut(as.numeric(prob[t, 3:102]),
            limits[[t]],
            labels = c(1:NLevels),
            right = TRUE)
    for (j in 1:10) {
        index = (j - 1) * 10 + 1:10
        r <- pf[[t]][index]
        m[[t]] <- rbind(m[[t]], r)
    }
}

for (i in 1:200) {
    m[[i]][is.na(m[[i]])] <- 0
}

#### desenez probabilitatile######

## 1. imi trebuie conturul fiecarei tile
### datatile[i] contine conturul tilei i

datatile <- list()
x <- list()
y <- list()
for (i in 0:99) {
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
persons <- persons[(persons[, 2] == 2), ]

ploturi_pers <-
    data.frame(t = numeric(), x = numeric(), y = numeric())

for (t in 1:200) {
    ploturi_pers[t, 1] <- t
    ploturi_pers[t, 2] <- persons[persons[, 1] == t - 1, ][1, 3]
    ploturi_pers[t, 3] <- persons[persons[, 1] == t - 1, ][1, 4]
}

ploturi_poligoane <-
    data.frame(t = numeric(), x = numeric(), y = numeric())
for (t in 1:200) {
    poligons <- list()
    for (i in 0:99) {
        nr <- floor(i /  grid$No.Tiles.X)
        nc <- floor(i - nr * grid$No.Tiles.X)
        if (m[[t]][nr + 1, nc + 1] == NLevels) {
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
        tmp <-
            rbind(
                tmp,
                c(
                    t,
                    uniune@polygons[[1]]@Polygons[[1]]@coords[j, 1],
                    uniune@polygons[[1]]@Polygons[[1]]@coords[j, 2]
                )
            )
        colnames(tmp) <- c("t", "x", "y")
    }
    ploturi_poligoane <- rbind(ploturi_poligoane, tmp)
}

df <- merge(ploturi_pers,
            ploturi_poligoane,
            by.x = "t",
            by.y = "t")

remove(p)
p <-
    ggplot() + geom_polygon(aes(x = datapoly[, 1], y = datapoly[, 2]),
                            fill = "#7D7D7D" ,
                            alpha = 0.5)
p <- p + geom_point(
    shape = 8,
    size = 6,
    data = antennas,
    aes(x = antennas[, 3], y = antennas[, 4]),
    colour = "#CC0000"
)
p <-
    p + scale_y_continuous(breaks = gridpointsy, minor_breaks = NULL)
p <-
    p + scale_x_continuous(breaks = gridpointsx, minor_breaks = NULL)
p <- p + guides(size = FALSE) + theme_bw()
p <- p + xlab(label = "Longitude") + ylab("Latitude")
p <- p + geom_point(data = df,
                    aes(x = df[, 2], y = df[, 3]),
                    size = 4,
                    shape = 4)
p <-
    p + geom_polygon(data = df, aes(x = df[, 4], y = df[, 5]), alpha = 0.05)
p <-
    p + transition_states(df[, 1], transition_length = 1, state_length = 1) + shadow_wake(wake_length = 0.025, alpha = FALSE)

options(gganimate.dev_args = list(width = 600, height = 600))
movie2 <- animate(
    p,
    nframes = 400,
    renderer = ffmpeg_renderer(),
    rewind = FALSE,
    duration = 40
)
anim_save(filename = "simulation2.mpeg", animation = last_animation())


#######in lucru################
ssteep = 0.2
smid = -92.5
power = 10
gamma = 3.87
distance = sqrt((7000 - 9500) ^ 2 + (2500 - 9500) ^ 2)
S0 = 30 + 10 * log10(power)
Sd = 10 * gamma * log10(distance)
S = S0 - Sd
result = 1.0 / (1 + exp(-ssteep * (S - smid)))
