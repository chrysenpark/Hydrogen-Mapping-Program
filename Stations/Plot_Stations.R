###### LOAD LIBRARIES #######
library("spatialEco")
library("sp")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("rgeos")
library("foreign")
library("ggmap")
library("tigris")
library("RColorBrewer")
library("RgoogleMaps")
library("raster")
library("dplyr")
library("stringr")
library("tidyr")
#############################

######## WORKING DIRECTORY AND FILES TO USE ###########

setwd("C:\\Users\\chrysen7.stu\\Documents\\CERCProject\\Stations")
fileName <- "stations_cropped.txt"
Canada_boundaries_map <- readOGR(dsn="C:\\Users\\chrysen7.stu\\Documents\\CERCProject\\Stations\\AdminBoundary", layer="AdminBoundary")    
######################################################

######### PARAMETERS #############
#Timestep of interest ('t' column): the timestep you want to plot
timestep_of_interest <- 2

#Station capacity in kilograms ('s' column)
SC_1 <- 500 
SC_2 <- 1000
SC_3 <- 1500

#Longitudes and latitudes for your 8 locations
Surrey_longitude <- -122.836753
Surrey_latitude <- 49.176224
Vancouver_longitude <- -123.141073
Vancouver_latitude <- 49.262644
Burnaby_longitude <- -122.990101
Burnaby_latitude <- 49.254084
Langley_longitude <- -122.660009
Langley_latitude <- 49.103392
Richmond_longitude <- -123.135495
Richmond_latitude <- 49.164603
MapleRidge_longitude <- -122.600190
MapleRidge_latitude <- 49.218434
Coquitlam_longitude <- -122.841745
Coquitlam_latitude <- 49.255372
Delta_longitude <- -123.046889
Delta_latitude <- 49.088889
NorthVan_longitude <- -123.073003
NorthVan_latitude <- 49.322245
WestVan_longitude <- -123.157104
WestVan_latitude <- 49.334125
#############################

############# CROP THE SHAPEFILE #################
MetroVan_map <- spTransform(Canada_boundaries_map, CRS("+proj=longlat +datum=WGS84"))
#Crop the shapefile to just the Greater Vancouver area
x_coord <- c(-123.35,  -123.35,  -122.3, -122.3, -123.35) #This is longitude
y_coord <- c(49, 49.39, 49.39, 49, 49) #This is latitude
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- CRS("+proj=longlat +datum=WGS84") #Convert to same lat/long 
MetroVan_map_cropped <- crop(MetroVan_map, sps) #Crop the map
#################################################

#plot(MetroVan_map_cropped)
############### CLEAN DATA AND PUT INTO DATAFRAME FORMAT ############
#Read text files as dataframe, and split up all the elements into single columns
dat <- readLines(fileName)
dat1 <- gsub("\\[|\\]", "", dat) #Remove the square brackets from the strings
dat2 <- as.data.frame(do.call(rbind, strsplit(dat1, split=",")), stringsAsFactors=FALSE)
dat3 <- extract(dat2, V1, into = c("ID", "s"), "(.{2})(.{1})", remove=TRUE) #Separate the first column into YO/YD and the 's' column
dat3$NumberOfStations <- lapply(strsplit(as.character(dat3$V4), "\\="), "[", 2)
dat3$V4 <- lapply(strsplit(as.character(dat3$V4), "\\="), "[", 1)
colnames(dat3) <- c("ID","s","d","g","t","NumberOfStations") #Rename columns
dat3["long"] <- NA; dat3["lat"] <- NA

#Add long/lat information based on 'g' column
dat3$long[dat3$g == 1] <- Surrey_longitude; dat3$lat[dat3$g == 1] <- Surrey_latitude;
dat3$long[dat3$g == 2] <- Vancouver_longitude; dat3$lat[dat3$g == 2] <- Vancouver_latitude;
dat3$long[dat3$g == 3] <- Burnaby_longitude; dat3$lat[dat3$g == 3] <- Burnaby_latitude;
dat3$long[dat3$g == 4] <- Langley_longitude; dat3$lat[dat3$g == 4] <- Langley_latitude;
dat3$long[dat3$g == 5] <- Richmond_longitude; dat3$lat[dat3$g == 5] <- Richmond_latitude;
dat3$long[dat3$g == 6] <- MapleRidge_longitude; dat3$lat[dat3$g == 6] <- MapleRidge_latitude;
dat3$long[dat3$g == 7] <- Coquitlam_longitude; dat3$lat[dat3$g == 7] <- Coquitlam_latitude;
dat3$long[dat3$g == 8] <- Delta_longitude; dat3$lat[dat3$g == 8] <- Delta_latitude;
dat3$long[dat3$g == 9] <- NorthVan_longitude; dat3$lat[dat3$g == 9] <- NorthVan_latitude;
dat3$long[dat3$g == 10] <- WestVan_longitude; dat3$lat[dat3$g == 10] <- WestVan_latitude;

dat4 <- dat3 #Create duplicate of dataframe

#Attribute the correct station capacity depending on numbers
dat4$s[dat4$s == 1] <- SC_1; dat4$s[dat4$s == 2] <- SC_2; dat4$s[dat4$s == 3] <- SC_3
dat4$s <- as.numeric(as.character(dat4$s))

YOstations <- subset(dat4,ID=='YO')
YDstations <- subset(dat4,ID=='YD')

#Start manipulating the YO and YD stations for plotting
YOstations_plotting <- YOstations
YDstations_plotting <- YDstations

#Delete any entries where there are no stations, so that you don't plot them (column 'NumberOfStations')
YOstations_plotting <- YOstations_plotting[(YOstations_plotting$NumberOfStations > 0),]
YDstations_plotting <- YDstations_plotting[(YDstations_plotting$NumberOfStations > 0),]

#Only choose the entries where the timestep is of interest for this particular plot (column 't')
YOstations_plotting <- YOstations_plotting[(YOstations_plotting$t == timestep_of_interest),]
YDstations_plotting <- YDstations_plotting[(YDstations_plotting$t == timestep_of_interest),]

#Generate a list of random numbers between 0.999 and 1.001, to multiply the long/lats so the points are randomly distributed
#For YOstations_plotting

multipliers <- seq(-.05,.05,.005)

YOmultipliers <- multipliers[1:nrow(YOstations_plotting)]
YOstations_plotting$long <- YOstations_plotting$long+YOmultipliers
YOstations_plotting$lat <- YOstations_plotting$lat+.012


#For YDstations_plotting

multipliers <- seq(-.05,.05,.005)

YDmultipliers <- multipliers[1:nrow(YDstations_plotting)]
YDstations_plotting$long <- YDstations_plotting$long+YDmultipliers
YDstations_plotting$lat <- YDstations_plotting$lat+.012

print(YDstations_plotting$d)
#Types of hydrogen
YOstations_onsite <- YOstations_plotting[(YOstations_plotting$d == 1),]
YDstations_onsite <- YDstations_plotting[(YDstations_plotting$d == 1),]
YOstations_gas <- YOstations_plotting[(YOstations_plotting$d == 2),]
YDstations_gas <- YDstations_plotting[(YDstations_plotting$d == 2),]
YOstations_liquid <- YOstations_plotting[(YOstations_plotting$d == 3),]
YDstations_liquid <- YDstations_plotting[(YDstations_plotting$d == 3),]

YDnumOnSite <- nrow(YDstations_onsite)
YDnumGas <- nrow(YDstations_gas)
YDnumLiquid <- nrow(YDstations_liquid)

####### PLOTTING ####### 


map_plot <- ggplot(MetroVan_map_cropped, aes(long, lat, group=group))
map_plot <- map_plot +  geom_polygon(aes(x = long, y = lat, group = group),
                                     color = "black")

map_plot <- map_plot + geom_point(data=YDstations_onsite, aes(x=long, y=lat, size=s, fill=d), shape=21, alpha=0.8, inherit.aes=FALSE)
map_plot <- map_plot + geom_point(data=YDstations_gas, aes(x=long, y=lat, size=s, fill=d), shape=21, alpha=0.8, inherit.aes=FALSE)
map_plot <- map_plot + geom_point(data=YDstations_liquid, aes(x=long, y=lat, size=s, fill=d), shape=21, alpha=0.8, inherit.aes=FALSE)

map_plot <- map_plot + geom_text(data=YDstations_plotting,
                                aes(x= long,y=lat, label= paste("   x", as.character(YDstations_plotting$NumberOfStations), sep= "")), angle= 0, hjust=0,size= 3,color="black"
                                ,inherit.aes = FALSE)
map_plot <- map_plot + scale_size_area(name= "Capacity", breaks = c(500,1000,1500))
map_plot <- map_plot + scale_fill_discrete(name = "Hydrogen Type", breaks =c(1,2,3))
                                         

map_plot <- map_plot +  annotate("text", label = "Surrey", x = Surrey_longitude, y= Surrey_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Vancouver", x = Vancouver_longitude, y= Vancouver_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Burnaby", x = Burnaby_longitude, y = Burnaby_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Langley", x = Langley_longitude, y = Langley_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Richmond", x = Richmond_longitude, y = Richmond_latitude, size= 3,color = "white")
map_plot <- map_plot +  annotate("text", label = "Maple Ridge", x = MapleRidge_longitude, y = MapleRidge_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Coquitlam", x = Coquitlam_longitude, y = Coquitlam_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "Delta", x = Delta_longitude, y = Delta_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "North Vancouver", x = NorthVan_longitude, y = NorthVan_latitude,size= 3, color = "white")
map_plot <- map_plot +  annotate("text", label = "West Vancouver", x = WestVan_longitude, y = WestVan_latitude,size= 3, color = "white")
map_plot <- map_plot + labs(title="Station capacities")
map_plot <- map_plot + coord_equal() 
map_plot <- map_plot + theme(
      panel.grid = element_blank())
print(map_plot)



####### SAVING PLOT #######
ggsave("StationCapacities.pdf", map_plot, width = 12.25, height = 12.15, dpi = 1200)
