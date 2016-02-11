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
lnd[, sel]@data
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

##### Attribute data #####
names(lnd)
summary(lnd)

##### Part 3: Creating and manipulating spatial data #####
# create a SpatialPoints object
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))
mat <- as.matrix(df) # create matrix object with as.matrix
mat
sp1 <- SpatialPoints(coords = mat)

class(sp1)

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)
spdf

##### Projections: setting and transforming CRS in R #####
# CRS = coordinate reference system

# Setting a CRS
proj4string(lnd)
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
# rm(lnd84)

##### Attribute Joins #####
# starting over with london data
lnd <- readOGR(dsn = "data", "london_sport")
plot(lnd)
nrow(lnd)

# We will join non-spatial crime data to the lnd object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv", 
                       stringsAsFactors = FALSE)

head(crime_data)
head(crime_data$CrimeType)

# Extract theft and handling
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft)

# Calculate sum of the crime count for each district
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)
head(crime_ag)

# Compare name columns
lnd$name %in% crime_ag$Borough
unique(lnd$name)
unique(crime_ag$Borough)

# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough]

crime_ag$Borough[!crime_ag$Borough %in% lnd$name]

# Quick test of %in% operator
1:10 %in% c(9,8,7,6,1)

# Challenge - identify the number of crimes taking place in borough 'NULL'
crime_ag$Borough %in% lnd$name
crime_ag$Borough[!crime_ag$Borough %in% lnd$name]

library(dplyr)

crime_ag %>% filter(Borough == "NULL") 

# Joining datasets
head(lnd$name)
head(crime_ag$Borough)
crime_ag <- rename(crime_ag, name = Borough)
lnd@data <- left_join(lnd@data, crime_ag)

lnd@data

# Skip plotting with tmap, because it won't load
# (need dev version of raster, which i don't want to install)
library("tmap")

##### Clipping and spatial joins #####
stations <- readOGR(dsn = "data", layer = "lnd-stns")
proj4string(stations)
proj4string(lnd)
bbox(stations)
bbox(lnd)

# Create reprojected stations object
stations27700 <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))
stations <- stations27700
rm(stations27700)
plot(lnd)
points(stations)

# Many points outside of the map, need to 'clip' the points
# Two functions available:
# sp::over
# rgeos::gIntersects

# Use sp::over
stations_backup <- stations

# sp::over is simply the square brackets:
stations <- stations_backup[lnd, ] # 'output all stations within lnd object bounds'
plot(stations)

# This is the long way:
sel <- over(stations_backup, lnd)
stations2 <- stations_backup[!is.na(sel[,1]), ]
summary(sel)
summary(stations2)

##### Spatial aggregation #####
stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length)
stations_agg@data # gives the number of stations per borough

lnd$n_points <- stations_agg$CODE
lnd$n_points

lnd_n <- aggregate(stations["NUMBER"], by = lnd, FUN = mean)
lnd_n@data

brks <- quantile(lnd_n$NUMBER)
brks

labs <- grey.colors(n = 4)
labs

q <- cut(lnd_n$NUMBER, brks, labels = labs, include.lowest = TRUE)
q
summary(q)

qc <- as.character(q) # conver to character class to plot
qc

plot(lnd_n, col = qc)
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright")

# new vector containing the area of each borough
areas <- sapply(lnd_n@polygons, function(x) x@area)
areas

plot(lnd_n$NUMBER, areas)
plot(1:10, pch = 1:10)

levels(stations$LEGEND) # see A roads and rapid transit stations
sel <- grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting
sel

sym <- as.integer(stations$LEGEND[sel]) # symbols
sym

points(stations[sel, ], pch = sym)
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))

##### Making maps with ggplot2 #####

library(ggplot2)

p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))
p + geom_point(aes(color = Partic_Per, size = Pop_2001)) + 
  geom_text(size = 2, aes(label = name))

# ggmap requires spatial data to be supplied as a data.frame, using fortify()
# fortify() extracts Spatial* objects as a dataframe
# either maptools or rgeos need to be installed

library(rgeos)
lnd_f <- fortify(lnd)
head(lnd_f)

lnd$id <- row.names(lnd)
head(lnd@data, n = 2)

# Join attribute data to lnd_f by id
lnd_f <- left_join(lnd_f, lnd@data)

head(lnd_f)

# now use ggplot to make the map
map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) + 
  geom_polygon() + 
  coord_equal() + 
  labs(x = "Easting (m)", y = "Northing (m)", 
       fill = "% Sports\nParticipation") + 
  ggtitle("London Sport Participation")
map

map + scale_fill_gradient(low = "white", high = "black")

##### Making maps with leaflet #####
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)

# for a tutorial, see:
# robinlovelace.net/r/2015/02/01/leaflet-r-package.html.

##### Faceting for maps #####
london_data <- read.csv("data/census-historic-population-borough.csv")
library(tidyr)
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy)

# merge population data with london borough geometry
head(lnd_f, 2)

ltidy <- rename(ltidy, ons_label = Area.Code)
lnd_f <- left_join(lnd_f, ltidy)

# rename date variable
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)

ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

