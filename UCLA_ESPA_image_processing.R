library(bfastSpatial)
library(raster)
library(rgdal)
library(tools)
library(ggplot2)
library(ggpmisc)
library(GISTools)
library(maps)
library(RColorBrewer)
library(tcltk)
library(beepr)
source("C:/Users/fyousef/Dropbox/R-scripts/fmatch.R")

##========================== Read Me First ======================
# Last Modifed: 3/11/2017
# 1) Both the raster and shapefilles show have the same coordinate sytem
# 2) The extent of shapefile has to be inside  the extent of the raster file
# 3) To mute the verbse, use shift+control+S
# 4) This has a dependecy code (fmatch.R) to run properly. The address is hardcoded above


## ========================= Directories =========================
## For Ubuntu
#wd <- "/home/mooddy/Documents/CO-spring"
#sigdir <- "/home/mooddy/Documents/sig"

## For Windows
product <- 'ndvi'
shpname <- "KS4"
nvf <- "N"
month <- "July"
fldname <- fmatch(shpname)
datadir <- paste0("C:/Users/fyousef/Documents/ESPA_data/Raw_Monthly/NDVI-other/", fldname,'/',month)
wd <- paste0("C:/Users/fyousef/Documents/ESPA_data/",product, "/4-NvF/",month)
crop_file<- paste0("C:/Users/fyousef/Documents/ESPA_data/extent/up_down-wind/All/NDW-FDW/",shpname, "_", nvf, ".shp")

## Set the location of output and intermediary directories 
dirout <- file.path(paste0(datadir, "/processed//")) #two slashes means an actual slash
sdir <- file.path(paste0(datadir, "/Stack"))
figtab <- file.path(paste0(wd, "/Graphs//"))
slpdir <- file.path(paste0(wd, "/Slope//"))
pvldir <- file.path(paste0(wd, "/P_value//"))
meandir <- file.path(paste0(wd, "/Means//"))
sigdir <- file.path(paste0(wd, "/SigSlp//"))
tabdir <- file.path(paste0(wd, "/Tables//"))

dirlist <- c(dirout, sdir, figtab, slpdir, pvldir, meandir, sigdir, tabdir)

print("Creating Output Directories")
for (dir in dirlist) {
  dir.create(dir, showWarning=FALSE, recursive=TRUE)
}

## Read Shapefile. Note: the dsn is the directory (without a trailing backslash) 
## and the layer is the shapefile name without the .shp.
## For each shape file a new directory is build for all the outputs
crop_name <- file_path_sans_ext(basename(crop_file))
crop_obj <- readOGR(dsn=crop_file, crop_name)




## Get list of test data files
list <- list.files(datadir, full.names=TRUE, pattern = "*.gz")
fcount <- length(list)
print(paste("Total number of files is:", fcount))
## ========================IMAGE PROCESSING================
## Generate (or extract, depending on whether the layer is already in the archive or not) NDVI for the first archive file, the e= argument below crops that dataset to the specified extent in crop_obj
## To do savi, use the L argument to specify a number between 0 and 1. Remove it for other indecies
t=1
for (m in list){
  print(paste0("Now Processing # ", t, ":", m))
  processLandsat(x=m, vi=product, outdir=dirout, srdir=sdir, delete=TRUE, mask='fmask', keep=0, overwrite=TRUE, e=extent(crop_obj))
  t=t+1
}
print('===================== Raster Info ======================')
## ===================== Raster Info ======================
list2 <- list.files(dirout, pattern=glob2rx(paste0(product,'*','.grd')), full.names=TRUE)
fcount <- length(list2)
print(paste("Total number of files is:", fcount))
print("========================= TIME STACK ============https://www.youtube.com/watch?v=7uEZl-YvhPg==========")
## ========================TIME STACK =====================
## Create a timestack for timeseries
## Mask out the date with the polygone

t=1
d <- raster()
for (x in list2) {
  d <- mask(raster(x), crop_obj) #Mask the files
  time <- getSceneinfo(x)$date
  d <- setZ(x=d, z=time)
  names(d) <- row.names(getSceneinfo(x))
  writeRaster(d, paste0(dirout,product,".",names(d)), overwrite=TRUE)
  print(paste0("Processing ", t, " of ", fcount, ". Filename: ",names(d)))
  t=t+1
}

list3 <- list.files(dirout, pattern=glob2rx(paste0(product,'*','.grd')), full.names=TRUE)
## Make a stack

stackName <- file.path(sdir, paste0(crop_name,'_',product,'_stack.grd'))

## Stack the layers, the stack object has a time component!
ESPA_VI <- timeStack(x=list3, filename=stackName, datatype='INT2S', overwrite=TRUE)


#plot(ESPA_VI)
#hist(ESPA_VI)
print("====================== Pixel-Based Trend Analysis ======================")
## ====================== Pixel-Based Trend Analysis ======================
## Now the stack layer is ready to be used for any kind of processing
## Trend analysis , .....

##Calculate the slope of the linear line fitted to each pixel
fun_slope <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ ESPA_VI@z$time); summary(m)$coefficients[2] 
  }
}

##Calculate the p-value of the linear line fitted to each pixel
fun_pvalue <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ ESPA_VI@z$time); summary(m)$coefficients[8] 
  }
}

print("Calculating the Slope")
slope <- calc(ESPA_VI, fun_slope)
print("")
print("Calculating the p-value")
pvalue <- calc(ESPA_VI, fun_pvalue)

## Mask out insignificant slopes
m = c(0, 0.05, 1, 0.05, 1, 0)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask = reclassify(pvalue, rclmat)
fun_mask=function(x) { x[x<1] <- NA; return(x)}
p.mask.NA = calc(p.mask, fun_mask)
trend.sig = mask(slope, p.mask.NA)
print("========================= Plotting and etc. ====================")
## ========================= Plotting and etc. ====================
## Calculate the slope and p-value of the linear model fitted to each pixel

## Write the slope to a file and plot
writeRaster(slope, paste0(slpdir,crop_name), format="GTiff")
png(paste0(figtab, crop_name, "_Slope.png"), width=1500, height=1000, res=120)
plot(slope, main=crop_name, col=colorRampPalette(brewer.pal(9,"RdYlGn"))(100))
SpatialPolygonsRescale(layout.north.arrow(1), offset= c(425000,4480000), scale = 10000, plot.grid=F)
SpatialPolygonsRescale(layout.scale.bar(), offset= c(450000,4480000), scale= 30000, fill=c("transparent", "black"), plot.grid= F)
dev.off()

## Write the pvalues to a file and plot
writeRaster(pvalue, paste0(pvldir,crop_name), format="GTiff")
png(paste0(figtab, crop_name, "_P-value.png"), width=1500, height=1000, res=120)
plot(pvalue, main=crop_name, col=colorRampPalette(brewer.pal(9,"RdYlGn"))(100))
SpatialPolygonsRescale(layout.north.arrow(1), offset= c(425000,4480000), scale = 10000, plot.grid=F)
SpatialPolygonsRescale(layout.scale.bar(), offset= c(450000,4480000), scale= 30000, fill=c("transparent", "black"), plot.grid= F)
dev.off()

## Write the MASKED slopes to a file and plot
writeRaster(trend.sig, paste0(sigdir, crop_name), format="GTiff",  overwrite=TRUE)


## pass this functino to summaryBrick
#customStat <- summaryBrick(s, fun=fun_slope)
#plot(customStat, main = "# of observations where NDVI > 0.7")

print("====================== Areal Linear Trend Analysis ==================")
## ====================== Areal Linear Trend Analysis ==================
## Summary of the stat across the entire image and a final dataframe
# Mean image
meanVI <- summaryBrick(ESPA_VI, fun=mean, na.rm=TRUE)
meanVI <- meanVI/10000 #Scale it back to NDVI
writeRaster(meanVI, paste0(meandir,crop_name), format="GTiff")
png(paste0(figtab,crop_name, "_mean_",product,".png"), width=1500, height=1000, res=120)
plot(meanVI, main=crop_name, col=colorRampPalette(brewer.pal(9,"RdYlGn"))(100))
SpatialPolygonsRescale(layout.north.arrow(1), offset= c(425000,4480000), scale = 10000, plot.grid=F)
SpatialPolygonsRescale(layout.scale.bar(), offset= c(450000,4480000), scale= 30000, fill=c("transparent", "black"), plot.grid= F)
text(487000, 4480900, "30KM", cex= 1)
dev.off()

# Mean dataframe
avg_NDVI <- as.data.frame(cellStats(ESPA_VI,mean))
avg_NDVI <- avg_NDVI/10000
names(avg_NDVI) <- "meanNDVI"
Datess <- ESPA_VI@z$time
avg_NDVI$Date <- Datess
write.table(avg_NDVI, file = paste0(tabdir,crop_name, ".csv"), row.names=FALSE , append = FALSE, sep=",")

##Plots the average VI over the entire image
## add the following line for the equation and R2 and fit line
#geom_line(colour = "black") + geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) + stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
my.formula <- y ~ x

png(paste0(figtab,crop_name,"_trend.png"), width=750, height=750, res=120)
p1 <- ggplot(avg_NDVI, aes(avg_NDVI$Date, avg_NDVI$meanNDVI), na.rm=TRUE) +
  geom_point(size=2,colour = "blue") + 
  ggtitle(paste(crop_name)) +
  xlab("Date") + ylab(paste("Mean", product)) + ylim(0,1) + theme(text = element_text(size=15)) + stat_fit_glance(method = "lm", method.args = list(formula = my.formula),geom = "text", aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))
p1
dev.off()

#Save the final distribution graph
png(paste0(figtab, crop_name, "_Hist.png"), width=770, height=750, res=120)
hist(trend.sig, main=paste(crop_name), xlim=c(-1,1), xlab = "", ylab="Frequency")
abline(v=0,col="red", lty=2)
dev.off()

beep(2)
shpname
rm(list=ls()) # To Remove all the variables
