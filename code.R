# Kathryn Brutigam 
#Final Project

#For this assignment, I want to utilize the package BBMM (Brownian Bridge 
#Movement Model). This analysis requires a minimum of location data in x,y 
#coordinate format (one UTM zone across all points will do),and the time lag 
#between successive points. For my dissertation research, I have captured and 
#satellite-marked 17 and 10 Sandhill cranes (Antigone canadensis) in winter 
#2014/15 and 2015/16, respectively. In 2014/15 four of the PTTs were equipped 
#with Argos satellite transmitters and the other 13 (and all PTTs in all years 
#following) were equipped with GPS/Argos satellite transmitters. I will 
#delineate home ranges using the Brownian Bridge Movement Model which is 
#appropriate for data with short, staggered time intervals.

################################################################################
################################################################################
library(rgdal)
library(BBMM)
require(survival)
library(maptools)
library(dplyr)
require(foreign)
require(lattice)
require(BBMM)

#To demonstrate my understanding of data frame reading and cleaning, I begin 
#with a .csv file that contains all 27 birds across all winters. I will 
#separate out each crane and each winter, and call on them separately in my bbmm 
#code blocks. The data frame is already in a pretty tidy format for these analyses, 
#but has many unneccesary columns that could be removed.
#First, I read in the data frame as a comma delimited file created in Excel.
#Next, I removed extra data that are not needed for these analyses. 
#I subset the data frame into separate winters (2014/15 and 2015/16) because in 
#my research, I will be comparing home ranges across years, breeding
#affiliations, and other population demographics. Then I selected columns that
#I want to use for future code.

AllCraneWinters <- read.csv("C:/Users/Kathy/Documents/Kathryn.Brautigam.Data.csv",
                            header = TRUE, sep = ",")

#In case I want to change these values as new information deems so.
location.errorGPS <- 18
TwosError <- 280
ThreesError <- 182
cell.sz <- 100
max.lg <- 600
##cleaning up 2014/15
BBMMW14allcol <- subset(AllCraneWinters, winter=="W14/15")
#head(BBMMW14allcol)
BBMMW14 <- subset(BBMMW14allcol, select = c(Bird.ID,PTT,Fix.Class,winter,
                                            ArcDT.MST,time.lag,fromx,fromy,
                                            distance.km,speed))
#head(BBMMW14)

##cleaning up 2015/16
BBMMW15allcol <- subset(AllCraneWinters, winter=="W15/16")
#head(BBMMW15allcol)
BBMMW15 <- subset(BBMMW15allcol, select = c(Bird.ID,PTT,Fix.Class,winter,
                                            ArcDT.MST,time.lag,fromx,fromy,
                                            distance.km,speed))
#head(BBMMW15)

##cleaning up 2016/17
BBMMW16allcol <- subset(AllCraneWinters, winter=="W16/17")
#head(BBMMW16allcol)
BBMMW16 <- subset(BBMMW16allcol, select = c(Bird.ID,PTT,Fix.Class,winter,
                                            ArcDT.MST,time.lag,fromx,fromy,
                                            distance.km,speed))
#head(BBMMW16)

#Next I created a list using the split function, separating the variable 'PTT' 
#into essentially individual satellite-tagged cranes. 
Crane14List <- split(BBMMW14, BBMMW14$PTT)
Crane15List <- split(BBMMW15, BBMMW15$PTT)
Crane16List <- split(BBMMW16, BBMMW16$PTT)


#Here I demonstrate my understanding and use of a package by creating brownian 
#bridge for each bird each winter, using final nomenclature = MULE##.YY, calls 
#on the list I created to crane by PTT 6 digit code. Cranes with GPS points have 
#an 18 meter error radii and cranes with Argos doplar points have a location 
#error weighted by the 2 different point types, 2 = 280 meter error radii, 
#3 = 182 meter error radii. I will calculate each crane winter's percentage per 
#point type and determine the weighted average error radii.

#Solving error radius for Argos PTTs, which do not have a constant error rate, 
#required weighted average for each of the 2 fix classes. For my analyses, I 
#only downloaded the top 2 fix classes, '2' and '3'. '3' is the best fix 
#class, with just a 182 meter elliptical error radius. '2' is the second best fix 
#class, with 280 meter elliptical error radius. There are four cranes with these
#PTTs, MULE06, 07, 10, and 11. Each will have different proportions of the 2 fix 
#classes. I attempted to find a function that would do this in one line, but I 
#ultimately found it quite simple to write a function to first solve the percentage
#of each fix class and then solve the weighted errors calling on the constants I
#defined in the first lines.GPS transmitters were put through rigorous testing 
#trials, and 95% of location points were found to be accurate within 18 meters, 
#and 99% within 30 meters, and 100% within 100 meters. The constants for '2's 
#and '3's represent the 95th percentile values from their respective field trials.


Percent <- function(x, n){length((which(x == n))) / length(x)}


#Now the four cranes
#MULE06 winter 14
fixClass2.6.14 <- Percent(Crane14List$'134891'$Fix.Class, 2)
fixClass2.6.14
fixClass3.6.14 <-Percent(Crane14List$'134891'$Fix.Class, 3)
fixClass3.6.14
location.error06.14 <- (TwosError*fixClass2.6.14 + ThreesError*fixClass3.6.14)
location.error06.14
#MULE06 winter 15
fixClass2.6.15 <- Percent(Crane15List$'134891'$Fix.Class, 2)
fixClass2.6.15
fixClass3.6.15 <-Percent(Crane15List$'134891'$Fix.Class, 3)
fixClass3.6.15
location.error06.15 <- (TwosError*fixClass2.6.15 + ThreesError*fixClass3.6.15)
location.error06.15

#MULE07 winter 14
fixClass2.7.14 <- Percent(Crane14List$'134890'$Fix.Class, 2)
fixClass2.7.14
fixClass3.7.14 <-Percent(Crane14List$'134890'$Fix.Class, 3)
fixClass3.7.14
location.error07.14 <- (TwosError*fixClass2.7.14 + ThreesError*fixClass3.7.14)
location.error07.14
#MULE06 winter 15
fixClass2.7.15 <- Percent(Crane15List$'134890'$Fix.Class, 2)
fixClass2.7.15
fixClass3.7.15 <-Percent(Crane15List$'134890'$Fix.Class, 3)
fixClass3.7.15
location.error07.15 <- (TwosError*fixClass2.7.15 + ThreesError*fixClass3.7.15)
location.error07.15

#MULE10 winter 14
fixClass2.10.14 <- Percent(Crane14List$'134892'$Fix.Class, 2)
fixClass2.10.14
fixClass3.10.14 <-Percent(Crane14List$'134892'$Fix.Class, 3)
fixClass3.10.14
location.error10.14 <- (TwosError*fixClass2.10.14 + ThreesError*fixClass3.10.14)
location.error10.14
#MULE10 winter 15
fixClass2.10.15 <- Percent(Crane15List$'134892'$Fix.Class, 2)
fixClass2.10.15
fixClass3.10.15 <-Percent(Crane15List$'134892'$Fix.Class, 3)
fixClass3.10.15
location.error10.15 <- (TwosError*fixClass2.10.15 + ThreesError*fixClass3.10.15)
location.error10.15

#MULE11 winter 14
fixClass2.11.14 <- Percent(Crane14List$'134893'$Fix.Class, 2)
fixClass2.11.14
fixClass3.11.14 <-Percent(Crane14List$'134893'$Fix.Class, 3)
fixClass3.11.14
location.error11.14 <- (TwosError*fixClass2.11.14 + ThreesError*fixClass3.11.14)
location.error11.14
#MULE10 winter 15
fixClass2.11.15 <- Percent(Crane15List$'134893'$Fix.Class, 2)
fixClass2.11.15
fixClass3.11.15 <-Percent(Crane15List$'134893'$Fix.Class, 3)
fixClass3.11.15
location.error11.15 <- (TwosError*fixClass2.11.15 + ThreesError*fixClass3.11.15)
location.error11.15


##The following code runs the brownian.bridge analyses on the x,y and time.lag 
#data for each separated crane and winter. I had to run these using R64 on a 
#64-bit system, and even then, each brownian.bridge took upwards of 30-60
#minutes to run. Because of this, I attached maps displaying what the contours
#look like projected on an x,y coordinate grid (UTM 14 WGS 1984).

##142739, MULE01###
#2014
BBMM01_W14 <- brownian.bridge(x=Crane14List$'142739'$fromx, 
                              y=Crane14List$'142739'$fromy, 
                              time.lag = Crane14List$'142739'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_01_14=bbmm.contour(BBMM01_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142739', plot=TRUE)

out<-data.frame(x=BBMM01_W14$x, y=BBMM01_W14$y, z=BBMM01_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_01_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))

writeOGR(obj = raster.contour, dsn = ".", layer = "MULE1.14", 
         driver= "ESRI Shapefile")

#2015
BBMM01_W15 <- brownian.bridge(x=Crane15List$'142739'$fromx, 
                              y=Crane15List$'142739'$fromy, 
                              time.lag = Crane15List$'142739'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_01_15=bbmm.contour(BBMM01_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142739', plot=TRUE)

out<-data.frame(x=BBMM01_W15$x, y=BBMM01_W15$y, z=BBMM01_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_01_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE1.15", 
         driver= "ESRI Shapefile")

##142740, MULE02###
#2014
BBMM02_W14 <- brownian.bridge(x=Crane14List$'142740'$fromx, 
                              y=Crane14List$'142740'$fromy, 
                              time.lag = Crane14List$'142740'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_02_14=bbmm.contour(BBMM02_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142740', plot=TRUE)

out<-data.frame(x=BBMM02_W14$x, y=BBMM02_W14$y, z=BBMM02_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_02_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE2.14", 
         driver= "ESRI Shapefile")

#2015
BBMM02_W15 <- brownian.bridge(x=Crane15List$'142740'$fromx, 
                              y=Crane15List$'142740'$fromy, 
                              time.lag = Crane15List$'142740'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)


contours_02_15=bbmm.contour(BBMM02_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142740', plot=TRUE)

out<-data.frame(x=BBMM02_W15$x, y=BBMM02_W15$y, z=BBMM02_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_02_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE2.15", 
         driver= "ESRI Shapefile")

##142736, MULE03###
#2014
BBMM03_W14 <- brownian.bridge(x=Crane14List$'142736'$fromx, 
                              y=Crane14List$'142736'$fromy, 
                              time.lag = Crane14List$'142736'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_03_14=bbmm.contour(BBMM03_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142736', plot=TRUE)

out<-data.frame(x=BBMM03_W14$x, y=BBMM03_W14$y, z=BBMM03_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_03_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE3.14", 
         driver= "ESRI Shapefile")

#2015
BBMM03_W15 <- brownian.bridge(x=Crane15List$'142736'$fromx, 
                              y=Crane15List$'142736'$fromy, 
                              time.lag = Crane15List$'142736'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours03_15=bbmm.contour(BBMM03_W15, levels = c(50, 75, 95), 
                           locations = Crane15List$'142736', plot=TRUE)

out<-data.frame(x=BBMM03_W15$x, y=BBMM03_W15$y, z=BBMM03_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours03_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE3.15", 
         driver= "ESRI Shapefile")

##142745, MULE04###
#2014
BBMM04_W14 <- brownian.bridge(x=Crane14List$'142745'$fromx, 
                              y=Crane14List$'142745'$fromy, 
                              time.lag = Crane14List$'142745'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_04_14=bbmm.contour(BBMM04_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142745', plot=TRUE)

out<-data.frame(x=BBMM04_W14$x, y=BBMM04_W14$y, z=BBMM04_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_04_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE4.14", 
         driver= "ESRI Shapefile")

#2015
BBMM04_W15 <- brownian.bridge(x=Crane15List$'142745'$fromx, 
                              y=Crane15List$'142745'$fromy, 
                              time.lag = Crane15List$'142745'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)


contours_04_15=bbmm.contour(BBMM04_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142745', plot=TRUE)

out<-data.frame(x=BBMM04_W15$x, y=BBMM04_W15$y, z=BBMM04_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_04_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE4.15", 
         driver= "ESRI Shapefile")

##142735, MULE05###
#2014
BBMM05_W14 <- brownian.bridge(x=Crane14List$'142735'$fromx, 
                              y=Crane14List$'142735'$fromy, 
                              time.lag = Crane14List$'142735'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_05_14=bbmm.contour(BBMM05_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142735', plot=TRUE)

out<-data.frame(x=BBMM05_W14$x, y=BBMM05_W14$y, z=BBMM05_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_05_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE5.14", 
         driver= "ESRI Shapefile")

#2015
BBMM05_W15 <- brownian.bridge(x=Crane15List$'142735'$fromx, 
                              y=Crane15List$'142735'$fromy, 
                              time.lag = Crane15List$'142735'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_05_15=bbmm.contour(BBMM05_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142735', plot=TRUE)

out<-data.frame(x=BBMM05_W15$x, y=BBMM05_W15$y, z=BBMM05_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_05_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE5.15", 
         driver= "ESRI Shapefile")

##134891, MULE06###
#2014
BBMM06_W14 <- brownian.bridge(x=Crane14List$'134891'$fromx, 
                              y=Crane14List$'134891'$fromy, 
                              time.lag = Crane14List$'134891'$time.lag[-1], 
                              location.error=location.error06.14, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_06_14=bbmm.contour(BBMM06_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'134891', plot=TRUE)

out<-data.frame(x=BBMM06_W14$x, y=BBMM06_W14$y, z=BBMM06_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_06_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE6.14", 
         driver= "ESRI Shapefile")

#2015
BBMM06_W15 <- brownian.bridge(x=Crane15List$'134891'$fromx, 
                              y=Crane15List$'134891'$fromy, 
                              time.lag = Crane15List$'134891'$time.lag[-1], 
                              location.error=location.error06.15, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_06_15=bbmm.contour(BBMM06_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'134891', plot=TRUE)

out<-data.frame(x=BBMM06_W15$x, y=BBMM06_W15$y, z=BBMM06_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_06_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE6.15", 
         driver= "ESRI Shapefile")


##134890, MULE07###
#2014
BBMM07_W14 <- brownian.bridge(x=Crane14List$'134890'$fromx, 
                              y=Crane14List$'134890'$fromy, 
                              time.lag = Crane14List$'134890'$time.lag[-1], 
                              location.error=location.error07.14, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_07_14=bbmm.contour(BBMM07_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'134890', plot=TRUE)

out<-data.frame(x=BBMM07_W14$x, y=BBMM07_W14$y, z=BBMM07_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_07_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE7.14", 
         driver= "ESRI Shapefile")

#2015
BBMM07_W15 <- brownian.bridge(x=Crane15List$'134890'$fromx, 
                              y=Crane15List$'134890'$fromy, 
                              time.lag = Crane15List$'134890'$time.lag[-1], 
                              location.error=location.error=location.error07.15, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_07_15=bbmm.contour(BBMM07_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'134890', plot=TRUE)

out<-data.frame(x=BBMM07_W15$x, y=BBMM07_W15$y, z=BBMM07_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_07_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE7.15", 
         driver= "ESRI Shapefile")

##142742, MULE08###
#2014
BBMM08_W14 <- brownian.bridge(x=Crane14List$'142742'$fromx,
                              y=Crane14List$'142742'$fromy, 
                              time.lag = Crane14List$'142742'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_08_14=bbmm.contour(BBMM08_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142742', plot=TRUE)

out<-data.frame(x=BBMM08_W14$x, y=BBMM08_W14$y, z=BBMM08_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_08_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE8.14", 
         driver= "ESRI Shapefile")

#2015
BBMM08_W15 <- brownian.bridge(x=Crane15List$'142742'$fromx, 
                              y=Crane15List$'142742'$fromy, 
                              time.lag = Crane15List$'142742'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_08_15=bbmm.contour(BBMM08_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142742', plot=TRUE)

out<-data.frame(x=BBMM08_W15$x, y=BBMM08_W15$y, z=BBMM08_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_08_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE8.15", 
         driver= "ESRI Shapefile")

##142738, MULE09###
#2014
BBMM09_W14 <- brownian.bridge(x=Crane14List$'142738'$fromx, 
                              y=Crane14List$'142738'$fromy, 
                              time.lag = Crane14List$'142738'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)
contours_09_14=bbmm.contour(BBMM09_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142738', plot=TRUE)

out<-data.frame(x=BBMM09_W14$x, y=BBMM09_W14$y, z=BBMM09_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_09_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE9.14", 
         driver= "ESRI Shapefile")

#2015
BBMM09_W15 <- brownian.bridge(x=Crane15List$'142738'$fromx, 
                              y=Crane15List$'142738'$fromy, 
                              time.lag = Crane15List$'142738'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_09_15=bbmm.contour(BBMM09_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142738', plot=TRUE)

out<-data.frame(x=BBMM09_W15$x, y=BBMM09_W15$y, z=BBMM09_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_09_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE9.15", 
         driver= "ESRI Shapefile")

##134892, MULE10###
#2014
BBMM10_W14 <- brownian.bridge(x=Crane14List$'134892'$fromx, 
                              y=Crane14List$'134892'$fromy, 
                              time.lag = Crane14List$'134892'$time.lag[-1], 
                              location.error=location.error10.14, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_10_14=bbmm.contour(BBMM10_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'134892', plot=TRUE)

out<-data.frame(x=BBMM10_W14$x, y=BBMM10_W14$y, z=BBMM10_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_10_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE10.14", 
         driver= "ESRI Shapefile")

#2015
BBMM10_W15 <- brownian.bridge(x=Crane15List$'134892'$fromx, 
                              y=Crane15List$'134892'$fromy, 
                              time.lag = Crane15List$'134892'$time.lag[-1], 
                              location.error=location.error10.15, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_10_15=bbmm.contour(BBMM10_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'134892', plot=TRUE)

out<-data.frame(x=BBMM10_W15$x, y=BBMM10_W15$y, z=BBMM10_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_10_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE10.15", 
         driver= "ESRI Shapefile")

##134893, MULE11###
#2014
BBMM11_W14 <- brownian.bridge(x=Crane14List$'134893'$fromx, 
                              y=Crane14List$'134893'$fromy, 
                              time.lag = Crane14List$'134893'$time.lag[-1], 
                              location.error=location.error11.14, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_11_14=bbmm.contour(BBMM11_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'134893', plot=TRUE)

out<-data.frame(x=BBMM11_W14$x, y=BBMM11_W14$y, z=BBMM11_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_11_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE11.14", 
         driver= "ESRI Shapefile")

#2015
BBMM11_W15 <- brownian.bridge(x=Crane15List$'134893'$fromx, 
                              y=Crane15List$'134893'$fromy, 
                              time.lag = Crane15List$'134893'$time.lag[-1], 
                              location.error=location.error11.15, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_11_15=bbmm.contour(BBMM11_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'134893', plot=TRUE)

out<-data.frame(x=BBMM11_W15$x, y=BBMM11_W15$y, z=BBMM11_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_11_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE11.15", 
         driver= "ESRI Shapefile")

##142744, MULE12###
#2014
BBMM12_W14 <- brownian.bridge(x=Crane14List$'142744'$fromx, 
                              y=Crane14List$'142744'$fromy, 
                              time.lag = Crane14List$'142744'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_12_14=bbmm.contour(BBMM12_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142744', plot=TRUE)

out<-data.frame(x=BBMM12_W14$x, y=BBMM12_W14$y, z=BBMM12_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_12_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE12.14", 
         driver= "ESRI Shapefile")

#2015
BBMM12_W15 <- brownian.bridge(x=Crane15List$'142744'$fromx, 
                              y=Crane15List$'142744'$fromy, 
                              time.lag = Crane15List$'142744'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_12_15=bbmm.contour(BBMM12_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142744', plot=TRUE)

out<-data.frame(x=BBMM12_W15$x, y=BBMM12_W15$y, z=BBMM12_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_12_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE12.15", 
         driver= "ESRI Shapefile")

##142741, MULE13###
#2014
BBMM13_W14 <- brownian.bridge(x=Crane14List$'142741'$fromx, 
                              y=Crane14List$'142741'$fromy, 
                              time.lag = Crane14List$'142741'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_13_14=bbmm.contour(BBMM13_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142741', plot=TRUE)

out<-data.frame(x=BBMM13_W14$x, y=BBMM13_W14$y, z=BBMM13_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_13_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE13.14", 
         driver= "ESRI Shapefile")

#2015
BBMM13_W15 <- brownian.bridge(x=Crane15List$'142741'$fromx, 
                              y=Crane15List$'142741'$fromy, 
                              time.lag = Crane15List$'142741'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_13_15=bbmm.contour(BBMM13_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142741', plot=TRUE)

out<-data.frame(x=BBMM13_W15$x, y=BBMM13_W15$y, z=BBMM13_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_13_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE13.15", 
         driver= "ESRI Shapefile")

##142743, MULE14###
#2014
BBMM14_W14 <- brownian.bridge(x=Crane14List$'142743'$fromx, 
                              y=Crane14List$'142743'$fromy, 
                              time.lag = Crane14List$'142743'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_14_14=bbmm.contour(BBMM14_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142743', plot=TRUE)

out<-data.frame(x=BBMM14_W14$x, y=BBMM14_W14$y, z=BBMM14_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_14_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE14.14", 
         driver= "ESRI Shapefile")

#2015
BBMM14_W15 <- brownian.bridge(x=Crane15List$'142743'$fromx, 
                              y=Crane15List$'142743'$fromy, 
                              time.lag = Crane15List$'142743'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_14_15=bbmm.contour(BBMM14_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142743', plot=TRUE)

out<-data.frame(x=BBMM14_W15$x, y=BBMM14_W15$y, z=BBMM14_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_14_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE14.15", 
         driver= "ESRI Shapefile")

##142737, MULE15###
#2014
BBMM15_W14 <- brownian.bridge(x=Crane14List$'142737'$fromx, 
                              y=Crane14List$'142737'$fromy, 
                              time.lag = Crane14List$'142737'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_15_14=bbmm.contour(BBMM15_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142737', plot=TRUE)

out<-data.frame(x=BBMM15_W14$x, y=BBMM15_W14$y, z=BBMM15_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_15_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE15.14", 
         driver= "ESRI Shapefile")

#2015
BBMM15_W15 <- brownian.bridge(x=Crane15List$'142737'$fromx, 
                              y=Crane15List$'142737'$fromy, 
                              time.lag = Crane15List$'142737'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_15_15=bbmm.contour(BBMM15_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142737', plot=TRUE)

out<-data.frame(x=BBMM15_W15$x, y=BBMM15_W15$y, z=BBMM15_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_15_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE15.15", 
         driver= "ESRI Shapefile")

##142746, MULE16###
#2014
BBMM16_W14 <- brownian.bridge(x=Crane14List$'142746'$fromx, 
                              y=Crane14List$'142746'$fromy, 
                              time.lag = Crane14List$'142746'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_16_14=bbmm.contour(BBMM16_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'142746', plot=TRUE)

out<-data.frame(x=BBMM16_W14$x, y=BBMM16_W14$y, z=BBMM16_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_16_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE16.14", 
         driver= "ESRI Shapefile")

#2015
BBMM16_W15 <- brownian.bridge(x=Crane15List$'142746'$fromx, 
                              y=Crane15List$'142746'$fromy, 
                              time.lag = Crane15List$'142746'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)


contours_16_15=bbmm.contour(BBMM16_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'142746', plot=TRUE)

out<-data.frame(x=BBMM16_W15$x, y=BBMM16_W15$y, z=BBMM16_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_16_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE16.15", 
         driver= "ESRI Shapefile")

##143698, MULE17###
#2014
BBMM17_W14 <- brownian.bridge(x=Crane14List$'143698'$fromx, 
                              y=Crane14List$'143698'$fromy, 
                              time.lag = Crane14List$'143698'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_17_14=bbmm.contour(BBMM17_W14, levels = c(50, 75, 95), 
                            locations = Crane14List$'143698', plot=TRUE)

out<-data.frame(x=BBMM17_W14$x, y=BBMM17_W14$y, z=BBMM17_W14$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_17_14$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE17.14", 
         driver= "ESRI Shapefile")

#2015
BBMM17_W15 <- brownian.bridge(x=Crane15List$'143698'$fromx, 
                              y=Crane15List$'143698'$fromy, 
                              time.lag = Crane15List$'143698'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)


contours_17_15=bbmm.contour(BBMM17_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143698', plot=TRUE)

out<-data.frame(x=BBMM17_W15$x, y=BBMM17_W15$y, z=BBMM17_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_17_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE17.15", 
         driver= "ESRI Shapefile")

##143699, MULE18###
#2015
BBMM18_W15 <- brownian.bridge(x=Crane15List$'143699'$fromx, 
                              y=Crane15List$'143699'$fromy, 
                              time.lag = Crane15List$'143699'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_18_15=bbmm.contour(BBMM18_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143699', plot=TRUE)

out<-data.frame(x=BBMM18_W15$x, y=BBMM18_W15$y, z=BBMM18_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_18_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE18.15", 
         driver= "ESRI Shapefile")

##143694, MULE19###
#2015
BBMM19_W15 <- brownian.bridge(x=Crane15List$'143694'$fromx, 
                              y=Crane15List$'143694'$fromy, 
                              time.lag = Crane15List$'143694'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_19_15=bbmm.contour(BBMM19_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143694', plot=TRUE)

out<-data.frame(x=BBMM19_W15$x, y=BBMM19_W15$y, z=BBMM19_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_19_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE19.15", 
         driver= "ESRI Shapefile")

##143697, MULE20###
#2015
BBMM20_W15 <- brownian.bridge(x=Crane15List$'143697'$fromx, 
                              y=Crane15List$'143697'$fromy, 
                              time.lag = Crane15List$'143697'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_20_15=bbmm.contour(BBMM20_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143697', plot=TRUE)

out<-data.frame(x=BBMM20_W15$x, y=BBMM20_W15$y, z=BBMM20_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_20_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE20.15", 
         driver= "ESRI Shapefile")

##143700, MULE21###
#2015
BBMM21_W15 <- brownian.bridge(x=Crane15List$'143700'$fromx, 
                              y=Crane15List$'143700'$fromy, 
                              time.lag = Crane15List$'143700'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_21_15=bbmm.contour(BBMM21_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143700', plot=TRUE)

out<-data.frame(x=BBMM21_W15$x, y=BBMM21_W15$y, z=BBMM21_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_21_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE21.15", 
         driver= "ESRI Shapefile")

##143701, MULE22###
#2015
BBMM22_W15 <- brownian.bridge(x=Crane15List$'143701'$fromx, 
                              y=Crane15List$'143701'$fromy, 
                              time.lag = Crane15List$'143701'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_22_15=bbmm.contour(BBMM22_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143701', plot=TRUE)

out<-data.frame(x=BBMM22_W15$x, y=BBMM22_W15$y, z=BBMM22_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_22_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE22.15", 
         driver= "ESRI Shapefile")

##152681, MULE23###
#2015
BBMM23_W15 <- brownian.bridge(x=Crane15List$'152681'$fromx, 
                              y=Crane15List$'152681'$fromy, 
                              time.lag = Crane15List$'152681'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_23_15=bbmm.contour(BBMM23_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'152681', plot=TRUE)

out<-data.frame(x=BBMM23_W15$x, y=BBMM23_W15$y, z=BBMM23_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_23_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE23.15", 
         driver= "ESRI Shapefile")

##143702, MULE24###
#2015
BBMM24_W15 <- brownian.bridge(x=Crane15List$'143702'$fromx, 
                              y=Crane15List$'143702'$fromy, 
                              time.lag = Crane15List$'143702'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_24_15=bbmm.contour(BBMM24_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'143702', plot=TRUE)

out<-data.frame(x=BBMM24_W15$x, y=BBMM24_W15$y, z=BBMM24_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_24_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE24.15", 
         driver= "ESRI Shapefile")

##152680, MULE25###
#2015
BBMM25_W15 <- brownian.bridge(x=Crane15List$'152680'$fromx, 
                              y=Crane15List$'152680'$fromy, 
                              time.lag = Crane15List$'152680'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_25_15=bbmm.contour(BBMM25_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'152680', plot=TRUE)

out<-data.frame(x=BBMM25_W15$x, y=BBMM25_W15$y, z=BBMM25_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_25_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE25.15", 
         driver= "ESRI Shapefile")

##152682, MULE26###
#2015
BBMM26_W15 <- brownian.bridge(x=Crane15List$'152682'$fromx, 
                              y=Crane15List$'152682'$fromy, 
                              time.lag = Crane15List$'152682'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_26_15=bbmm.contour(BBMM26_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'152682', plot=TRUE)

out<-data.frame(x=BBMM26_W15$x, y=BBMM26_W15$y, z=BBMM26_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_26_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE26.15", 
         driver= "ESRI Shapefile")

##152683, MULE27###
#2015
BBMM27_W15 <- brownian.bridge(x=Crane15List$'152683'$fromx, 
                              y=Crane15List$'152683'$fromy, 
                              time.lag = Crane15List$'152683'$time.lag[-1], 
                              location.error=location.errorGPS, 
                              cell.size = cell.sz, max.lag = max.lg)

contours_27_15=bbmm.contour(BBMM27_W15, levels = c(50, 75, 95), 
                            locations = Crane15List$'152683', plot=TRUE)

out<-data.frame(x=BBMM27_W15$x, y=BBMM27_W15$y, z=BBMM27_W15$probability)
out.raster<-rasterFromXYZ(out, crs = CRS("+proj=utm +zone=14 +datum=WGS84"), 
                          digits = 2)
raster.contour<-rasterToContour(out.raster, levels=contours_27_15$Z)
raster.contour<-spChFIDs(raster.contour, paste(c(50, 75, 95),"% Contour Line", 
                                               sep = ""))
writeOGR(obj = raster.contour, dsn = ".", layer = "MULE27.15", 
         driver= "ESRI Shapefile")

###############################################################################


#Using ggplot to display data and calculating and displaying statistics.
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

#Here I calculate the mean and standard deviations of x-coordinates, 
#y-coordinates, and distances between points for each crane, and I will use the 
#mean x,y values in one of my plots.
mean.Allcrane <- AllCraneWinters %>% group_by(PTT) %>% summarise (mean.dis = 
                  mean(distance.km), mean.x = mean(fromx), mean.y = mean(fromy))
mean.Allcrane


#head(mean.Allcrane)

#All winters with fewer columns in  a subset data frame.
BBMMwintersclean <- subset(AllCraneWinters, select = c(Bird.ID,PTT,Fix.Class,
                                                       winter,ArcDT.MST,
                                                       time.lag,fromx,fromy,
                                                       distance.km,speed))

###############################################################################
######Plots####################################################################
#Plotting data and differentiating with color by year.
#The library ggplot has a lot of capabilities and options to plot x,y data 
#associated with UTM x,y coordinates. First I plot the mean location of each
#crane, basically the center of all their movements. This is nonsense for plot
#data, but interesting capability nonetheless.

mid.location.ptt <- ggplot(mean.Allcrane, aes(mean.x, mean.y, color=PTT))
mid.location.ptt <- mid.location.ptt + geom_point(aes(colour = PTT), size = 2) + 
  ggtitle('Crane Midpoints') + 
  xlab("X coordinate") + ylab("Y coordinate")
mid.location.ptt

#Plotting data and differentiating with color by crane.
p <- ggplot(BBMMwintersclean, aes(fromx, fromy))
p <- p + geom_point(aes(colour = PTT), size = 0.5)  + ggtitle('Crane Locations') 
+ xlab("X coordinate") + 
  ylab("Y coordinate")
p
#Plotting data and differentiating with color by winter.
pp <- ggplot(BBMMwintersclean, aes(fromx, fromy))
pp <- pp + geom_point(aes(colour = winter), size = 0.5) + ggtitle('Winters') + 
  xlab("X coordinate") +
  ylab("Y coordinate")
pp  

#Next, I plot the points during winter 2014/15 and 2015/16 separately using 
#color gradient on the PTT variable (crane).
w14points <- ggplot(BBMMW14, aes(fromx, fromy))
w14points <- w14points + geom_point(aes(colour = PTT), size = 0.5) + 
  ggtitle('Winter 2014/15')+ 
  xlab("X coordinate") + ylab("Y coordinate")
w14points
#2015
w15points <- ggplot(BBMMW15, aes(fromx, fromy))
w15points <- w15points + geom_point(aes(colour = PTT), size = 0.5) + 
  ggtitle('Winter 2015/16') + 
  xlab("X coordinate") + ylab("Y coordinate")
w15points
#2016
w16points <- ggplot(BBMMW16, aes(fromx, fromy))
w16points <- w16points + geom_point(aes(colour = PTT), size = 0.5) + 
  ggtitle('Winter 2016/17') + 
  xlab("X coordinate") + ylab("Y coordinate")
w16points
