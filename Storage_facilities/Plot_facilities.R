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
library("installr")
##################################### WORKING DIRECTORY AND FILES TO USE ###########
setwd("C:\\Users\\chrysen7.stu\\Documents\\CERCProject\\Storage_Facilities")
fileName <- "Facilities_cropped.txt"
fileName2 <- "Transportation_cropped.txt"
Canada_boundaries_map <- readOGR(dsn="C:\\Users\\chrysen7.stu\\Documents\\CERCProject\\Storage_Facilities\\CD_2011", layer="CD_2011")    
######################################################

######### PARAMETERS #############
#Timestep of interest ('t' column): the timestep you want to plot
timestep_of_interest <- 7

#Plant capacity in kilograms ('s' column)
PC_1 <- 500 
PC_2 <- 1000
PC_3 <- 1500

#Longitudes and latitudes for your 12 location
FortNelson_longitude <- -122.6972
FortNelson_latitude <- 58.8050
FortStJohn_longitude <- -120.8464
FortStJohn_latitude <- 56.2524
PrinceGeorge_longitude <- -122.7497
PrinceGeorge_latitude <- 53.9171
WilliamsLake_longitude <- -122.1417
WilliamsLake_latitude <- 52.1417
Mica_longitude <- -118.5666644
Mica_latitude <- 52.0
PrinceRupert_longitude <- -130.3208
PrinceRupert_latitude <- 54.3150
Kamloops_longitude <- -120.3273
Kamloops_latitude <- 50.6745
Merritt_longitude <- -120.7862
Merritt_latitude <- 50.1113
Kelowna_longitude <- -119.4960
Kelowna_latitude <- 49.8880
Nelson_longitude <- -117.2948
Nelson_latitude <- 49.4928
Kimberley_longitude <- -115.9967
Kimberley_latitude <- 49.6652
Hope_longitude <- -121.4425
Hope_latitude <- 49.3830

#############################

############# CROP THE SHAPEFILE #################
BC_map <- spTransform(Canada_boundaries_map, CRS("+proj=longlat +datum=WGS84"))
#Crop the shapefile to just the Greater Vancouver area
x_coord <- c(-140.2, -140.2,  -110, -110, -140.2) #This is longitude
y_coord <- c(45, 60, 60, 45, 45) #This is latitude
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- CRS("+proj=longlat +datum=WGS84") #Convert to same lat/long 
BC_map_cropped <- crop(BC_map, sps)#Crop the map
#################################################

############### CLEAN DATA AND PUT INTO DATAFRAME FORMAT ############
#Read text files as dataframe, and split up all the elements into single columns
dat <- readLines(fileName)
dat1 <- gsub("\\[|\\]", "", dat) #Remove the square brackets from the strings
dat2 <- as.data.frame(do.call(rbind, strsplit(dat1, split=",")), stringsAsFactors=FALSE)
dat3 <- extract(dat2, V1, into = c("ID","c"), "(.{2})(.{1})", remove=TRUE) #Separate the first column into YO/YD and the 's' column
dat4 <- extract(dat3, V5, into = c("t", "NumberOfFacilities"), "(.{2})(.{1})", remove=TRUE) #Separate the first column into YO/YD and the 's' column
dat4$NumberOfFacilities <- lapply(strsplit(as.character(dat3$V5), "\\="), "[", 2)
dat4$t <- lapply(strsplit(as.character(dat3$V5), "\\="), "[", 1)
colnames(dat4) <- c("ID","c","y","d","g","t","NumberOfFacilities") #Rename columns
dat4["long"] <- NA; dat4["lat"] <- NA

#Read transportation text file and split up all the elements into single columns
transData <- readLines(fileName2)
transData1 <- gsub("\\[|\\]", "", transData)
transData2 <- as.data.frame(do.call(rbind, strsplit(transData1, split=",")), stringsAsFactors=FALSE)
transData3 <- extract(transData2, V1, into = c("ID","c"), "(.{4})(.{1})", remove=TRUE) #Separate the first column into YO/YD and the 's' column
transData4 <- extract(transData3, V6, into = c("t", "NumberOfTrucks"), "(.{2})(.{1})", remove=TRUE) #Separate the first column into YO/YD and the 's' column
transData4$NumberOfTrucks <- lapply(strsplit(as.character(transData3$V6), "\\="), "[", 2)
transData4$t <- lapply(strsplit(as.character(transData3$V6), "\\="), "[", 1)
colnames(transData4) <- c("ID","c","y","gp","g","s","t", "NumberOfTrucks") #Rename columns
transData4$NumberOfTrucks <- as.numeric(transData4$NumberOfTrucks)

#Add long/lat information based on 'g' column for facilities
dat4$long[dat4$g == 1] <- FortNelson_longitude; dat4$lat[dat4$g == 1] <- FortNelson_latitude;
dat4$long[dat4$g == 2] <- FortStJohn_longitude; dat4$lat[dat4$g == 2] <- FortStJohn_latitude;
dat4$long[dat4$g == 3] <- PrinceGeorge_longitude; dat4$lat[dat4$g == 3] <- PrinceGeorge_latitude;
dat4$long[dat4$g == 4] <- WilliamsLake_longitude; dat4$lat[dat4$g == 4] <- WilliamsLake_latitude;
dat4$long[dat4$g == 5] <- Mica_longitude; dat4$lat[dat4$g == 5] <- Mica_latitude;
dat4$long[dat4$g == 6] <- PrinceRupert_longitude; dat4$lat[dat4$g == 6] <- PrinceRupert_latitude;
dat4$long[dat4$g == 7] <- Kamloops_longitude; dat4$lat[dat4$g == 7] <- Kamloops_latitude;
dat4$long[dat4$g == 8] <- Merritt_longitude; dat4$lat[dat4$g == 8] <- Merritt_latitude;
dat4$long[dat4$g == 9] <- Kelowna_longitude; dat4$lat[dat4$g == 9] <- Kelowna_latitude;
dat4$long[dat4$g == 10] <- Nelson_longitude; dat4$lat[dat4$g == 10] <- Nelson_latitude;
dat4$long[dat4$g == 11] <- Kimberley_longitude; dat4$lat[dat4$g == 11] <- Kimberley_latitude;
dat4$long[dat4$g == 12] <- Hope_longitude; dat4$lat[dat4$g == 12] <- Hope_latitude;
dat5 <- dat4 #Create duplicate of dataframe

#Attribute the correct station capacity depending on numbers
dat5$c[dat5$c == 1] <- PC_1; dat5$c[dat5$c == 2] <- PC_2; dat5$c[dat5$c == 3] <- PC_3
dat5$c <- as.numeric(as.character(dat5$c))

#Assigning timestep to trucks
transData4 <- transData4[(transData4$t == timestep_of_interest),]

#Assigning trucks to each region (gp)
FortNelTrucks <- transData4[(transData4$gp == 1),]
FortJohnTrucks <- transData4[(transData4$gp == 2),]
PrinceGeorgeTrucks <- transData4[(transData4$gp == 3),]
WilliamsTrucks <- transData4[(transData4$gp == 4),]
MicaTrucks <- transData4[(transData4$gp == 5),]
PrinceRupertTrucks <- transData4[(transData4$gp == 6),]
KamloopsTrucks <- transData4[(transData4$gp == 7),]
MerrittTrucks <- transData4[(transData4$gp == 8),]
KelownaTrucks <- transData4[(transData4$gp == 9),]
NelsonTrucks <- transData4[(transData4$gp == 10),]
KimberleyTrucks <- transData4[(transData4$gp == 11),]
HopeTrucks <- transData4[(transData4$gp == 12),]


#Creating a set for Both YC AND YS stations
YCstorage <- subset(dat5,ID=='YC')
YSstorage <- subset(dat5,ID=='YS')


#Start manipulating the YC and YS storage facilities for plotting
#Only plotting one point to represent BOTH YC and YS facilities at the same location
YCSstorage_plotting <- YCstorage


#Delete any entries where there are no stations, so that you don't plot them (column 'NumberOfFacilities')
YCSstorage_plotting <- YCSstorage_plotting[(YCSstorage_plotting$NumberOfFacilities > 0),]


#Only choose the entries where the timestep is of interest for this particular plot (column 't')
YCSstorage_plotting <- YCSstorage_plotting[(YCSstorage_plotting$t == timestep_of_interest),]


random_multipliers <- runif(nrow(YCSstorage_plotting), 0.99, 1.01)
random_multipliers2 <- runif(nrow(YCSstorage_plotting), 0.99, 1.01)
YCSstorage_plotting$long <- YCSstorage_plotting$long*random_multipliers
YCSstorage_plotting$lat <- YCSstorage_plotting$lat


YCSstorage_onsite <- YCSstorage_plotting[(YCSstorage_plotting$d == 1),]
YCSstorage_gas <- YCSstorage_plotting[(YCSstorage_plotting$d == 2),]
YCSstorage_liquid <- YCSstorage_plotting[(YCSstorage_plotting$d == 3),]

YCSstorage_Electric <- YCSstorage_plotting[(YCSstorage_plotting$y == 1),]
YCSstorage_SMRwithCSS <- YCSstorage_plotting[(YCSstorage_plotting$y == 2),]
YCSstorage_SMR <- YCSstorage_plotting[(YCSstorage_plotting$y == 3),]


#MetroVancouver location 
metro <- data.frame(
  long = -123.0824,
  lat = 49.2501,
  stringsAsFactors = FALSE
)

#Liquified hydrogen facilities dataframe
liquid <- data.frame(
  long= YCSstorage_liquid$long,
  lat= YCSstorage_liquid$lat
)

#Gaseous hydrogen facilities dataframe
gas <- data.frame(
  long= YCSstorage_gas$long,
  lat= YCSstorage_gas$lat
)

#For Loop counters
gasNum <- 1:nrow(gas)
liquidNum <- 1:nrow(liquid)

####### PLOTTING ####### 
#plotting YC and YS stations

map_plot <- ggplot(BC_map_cropped, aes(long, lat, group=group))
map_plot <- map_plot + geom_polygon()
map_plot <- map_plot + geom_point(data=YCSstorage_Electric, aes(x=long, y=lat, size=c, fill=d), shape=23,  alpha=0.8, inherit.aes=FALSE)
map_plot <- map_plot + geom_point(data=YCSstorage_SMRwithCSS, aes(x=long, y=lat, size=c,fill=d), shape=23, alpha=0.8, inherit.aes=FALSE)
map_plot <- map_plot + geom_point(data=YCSstorage_SMR, aes(x=long, y=lat, size=c,fill=d), shape= 23, alpha=0.8, inherit.aes=FALSE)
map_plot <- map_plot + geom_point(data= metro, aes(x= long, y= lat), colour= "black", size=6, shape= 19, inherit.aes = FALSE)

#Creating print text for each region that include name and amount of trucks leaving that region

FortNeltext <- paste("Fort Nelson\ntrucks:", sum(FortNelTrucks$NumberOfTrucks))
FortJohntext <- paste("Fort St.John\ntrucks:", sum(FortJohnTrucks$NumberOfTrucks))
PrinceGeorgtext <- paste("Prince George\ntrucks:", sum(PrinceGeorgeTrucks$NumberOfTrucks))
WillLaketext <- paste("Williams Lake\ntrucks:", sum(WilliamsTrucks$NumberOfTrucks))
Micatext <- paste("Mica\nt:", sum(MicaTrucks$NumberOfTrucks))
PrinceRuptext <- paste("Prince Rupert\ntrucks:", sum(PrinceRupertTrucks$NumberOfTrucks))
Kamloopstext <- paste("Kamloops\nt:", sum(KamloopsTrucks$NumberOfTrucks))
Merritttext <- paste("Merrit\nt:", sum(MerrittTrucks$NumberOfTrucks))
Kelownatext <- paste("Kelowna\nt:", sum(KelownaTrucks$NumberOfTrucks))
Nelsontext <- paste("Nelson\nt:", sum(NelsonTrucks$NumberOfTrucks))
Kimbertext <- paste("Kimberley\nt:", sum(KimberleyTrucks$NumberOfTrucks))
Hopetext <- paste("Hope\nt:", sum(HopeTrucks$NumberOfTrucks))

#Labelling each region with amounts of trucks leaving it
map_plot <- map_plot +  annotate("text", label = FortNeltext, x= FortNelson_longitude, y= FortNelson_latitude+.5,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = FortJohntext, x = FortStJohn_longitude, y= FortStJohn_latitude+.5,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = PrinceGeorgtext, x = PrinceGeorge_longitude, y = PrinceGeorge_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = WillLaketext, x = WilliamsLake_longitude, y = WilliamsLake_latitude+.5,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Micatext, x = Mica_longitude-1, y = Mica_latitude, size= 2.5,color = "white")
map_plot <- map_plot +  annotate("text", label = PrinceRuptext, x = PrinceRupert_longitude+1.5, y = PrinceRupert_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Kamloopstext, x = Kamloops_longitude, y = Kamloops_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Merritttext, x = Merritt_longitude, y = Merritt_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Kelownatext, x = Kelowna_longitude, y = Kelowna_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Nelsontext, x = Nelson_longitude, y = Nelson_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Kimbertext, x = Kimberley_longitude, y = Kimberley_latitude,size= 2.5, color = "white")
map_plot <- map_plot +  annotate("text", label = Hopetext, x = Hope_longitude, y = Hope_latitude+.25,size= 2.5, color = "white")

#Adding lines from each facility to the greater vancouver region
#Creating a For loop making a matrix with 2 columns and (the number of Facilities) rows.
#The entire first column should be all Metro and the second should be each facility locations

for(i in gasNum) {
  map_plot <- map_plot + geom_line(data= rbind(gas[i,], metro), linetype = "longdash",
                                 aes(x=long, y=lat,color= "Gaseous"), inherit.aes = FALSE)
}  

for(i in liquidNum) {
  map_plot <- map_plot + geom_line(data= rbind(liquid[i,],metro), linetype = "longdash",
                                 aes(x=long, y=lat, color = "Liquified"), inherit.aes = FALSE)
}


#Constructing the Legend
map_plot <- map_plot + scale_size_area(name= "Capacity", breaks = c(500,1000,1500))
map_plot <- map_plot + scale_colour_manual(name = "Hydrogen Type \nTransported by Truck", 
                                           values = c(Gaseous="red", Liquified="blue")) 
map_plot <- map_plot + scale_fill_manual(name = "Plant Type",
                                         #labels = c("Electrolyzer","SMRwithCSS","SMRwithoutCSS"),
                                          values= c("yellow","lightblue", "lightgreen"))

map_plot <- map_plot + theme(panel.background = element_rect(fill = "white"),
                             panel.grid = element_blank()
                             )
map_plot <- map_plot + labs(title="Plants and Storage Facilities")
map_plot <- map_plot + coord_equal()

print(map_plot)

####### SAVING PLOT #######
ggsave("StorageFacilities.pdf", map_plot, width = 15.25, height = 20.15, dpi = 1200)
