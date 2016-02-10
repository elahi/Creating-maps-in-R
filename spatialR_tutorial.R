# Robin Elahi
# Intro to visualising spatial data in R
# Tutorial from Robin Lovelace's github
# https://github.com/Robinlovelace/Creating-maps-in-R

library(rgdal)

##### Load spatial data #####
lnd <- readOGR(dsn = "data", layer = "london_sport")
head(lnd)
head(lnd@data, n = 2)
mean(lnd$Partic_Per)
head(lnd@polygons[[1]]@Polygons[[1]]@coords, 3)

##### Basic plotting #####
plot(lnd@polygons[[1]]@Polygons[[1]]@coords)
plot(lnd)
plot(lnd@data)
lnd@data[lnd$Partic_Per < 15, ]

# Subset zones
sel <- lnd$Partic_Per > 20 & lnd$Partic_per < 25
lnd[, sel]
plot(lnd[sel, ])
head(sel)

plot(lnd, col = "lightgray")
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ], col = "turquoise", add = TRUE)

### Attribute data
summary(lnd)

##### Centroid challenge #####
library(rgeos)
plot(lnd, col = "grey")
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 

# method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,] # the selection is too big!
# test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer

# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Add text to the plot!
text(coordinates(cent_lnd), "Central\nLondon")

##### Selecting quadrants #####
# Create an outline of the London area by merging all of the polygons in the 
# lnd object
london = gUnaryUnion(lnd, lnd$dummy)
london = SpatialPolygonsDataFrame(london, data.frame(dummy = c("london")), 
                                  match.ID = FALSE)
summary(london)

# Find the centre of the london area
centrelondon = gCentroid(london, byid = TRUE)

# create coordinates to store the start and end points of the lines
c1 = c(centrelondon$x, centrelondon$x)
c2 = c(90, -90)
c3 = c(90, -90)
c4 = c(centrelondon$y,centrelondon$y)

# simple line strings using the created coordinates
L1 = Line(cbind(c1, c2))
L2 = Line(cbind(c3, c4))

# create the lines
Ls1 = Lines(list(L1), ID = "a")
Ls2 = Lines(list(L2), ID = "b")

# convert the lines into SpatialLines
Ls1 <- SpatialLines(LinesList = list(Ls1))
Ls2 <- SpatialLines(LinesList = list(Ls2))

# convert again into SpatialLinesDataFrame
Longitude = SpatialLinesDataFrame(Ls1, data.frame(Z = c("1", "2"), row.names = c("a","b")))
Latitude = SpatialLinesDataFrame(Ls2, data.frame(Z = c("1", "2"), row.names = c("a","b")))

# arguments to test whether or not a coordinate is east or north of the centre
east <- coordinates(lnd)[,1] > Longitude@lines[[1]]@Lines[[1]]@coords[,1][1]
north <- coordinates(lnd)[,2] > Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]

# test if the coordinate is east and north of the centre
lnd@data$quadrant[east & north] <- "northeast"

plot(lnd, border = "red")
plot(london, add = TRUE)

plot(london)
plot(Longitude, add = TRUE)
plot(Latitude)

summary(lnd)
lnd


##### Challenge: coloring quadrants #####
library(grid)
library(png)
grid.raster(readPNG("figure/lnd-quads.png"))

par(mfrow = c(1,2))

# plot the results
plot(london)
plot(lnd[east & north,],add = TRUE, col = "red" )

# place a grid over the object
llgridlines(lnd, lty= 3, side ="EN", offset = -0.5)

london = gUnaryUnion(lnd, lnd$dummy)
london = SpatialPolygonsDataFrame(london, data.frame(dummy = c("london")), 
                                  match.ID = FALSE)

centrelondon = gCentroid(london, byid = TRUE)

centreLEP = gCentroid(lnd, byid = TRUE)
coords <- cbind(centrelondon$y)

c1 = c(centrelondon$x, centrelondon$x)
c2 = c(90, -90)
c3 = c(90, -90)
c4 = c(centrelondon$y,centrelondon$y)

# simple line strings
L1 = Line(cbind(c1, c2))
L2 = Line(cbind(c3, c4))

Ls1 = Lines(list(L1), ID = "a")
Ls2 = Lines(list(L2), ID = "b")

Ls1 <- SpatialLines(LinesList = list(Ls1))
Ls2 <- SpatialLines(LinesList = list(Ls2))

Longitude = SpatialLinesDataFrame(Ls1, data.frame(Z = c("1", "2"), row.names = c("a","b")))
Latitude = SpatialLinesDataFrame(Ls2, data.frame(Z = c("1", "2"), row.names = c("a","b")))

east <- coordinates(lnd)[,1] > Longitude@lines[[1]]@Lines[[1]]@coords[,1][1]
north <- coordinates(lnd)[,2] > Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]
west <- coordinates(lnd)[,1] < Longitude@lines[[1]]@Lines[[1]]@coords[,1][1]
south <-coordinates(lnd)[,2] < Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]

lnd@data$quadrant[east & north] <- "northeast"
lnd@data$quadrant[west & north] <- "northwest"
lnd@data$quadrant[east & south] <- "southeast"
lnd@data$quadrant[west & south] <- "southwest"

plot(lnd)
plot(lnd[east & north,],add = TRUE, col = "red" )
plot(lnd[west & north,],add = TRUE, col = "blue" )
plot(lnd[east & south,],add = TRUE, col = "green" )
plot(lnd[west & south,],add = TRUE, col = "yellow" )

llgridlines(lnd, lty= 3, side ="EN", offset = -0.5)

par(mfrow=c(1,1)) # return to default par


########################################
### Part 3: Creating and manipulating spatial data

# create a SpatialPoints object
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))
mat <- as.matrix(df) # create matrix object with as.matrix
sp1 <- SpatialPoints(coords = mat)

class(sp1)

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)

# Projections: setting and transforming CRS in R
# CRS = coordinate reference system

# Setting a CRS
proj4string(lnd) <- NA_character_
proj4string(lnd) <- CRS("+init=epsg:27700")

# List of available EPSG codes
EPSG <- make_EPSG() # create df of codes
head(EPSG)

EPSG[grepl("WGS 84$", EPSG$note), ]

# Reproject using WGS84
lnd84 <- spTransform(lnd, CRS("+init=epsg:4326"))

# Save new object
saveRDS(object = "lnd84", file = "data/lnd84.Rds")
# Remove old object from environment
rm(lnd84)

### Attribute Joins

########################################


