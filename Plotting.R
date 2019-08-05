library(ggplot2)
library(raster)
library(ggpmisc) 
library(tools)
library(xts)
library(stringr)
library(plyr)


sigdir <- "C:/Users/fyousef/Documents/ESPA_data/ndvi/1-Spring/SigSlp"
flist <- list.files(sigdir, full.names = TRUE, pattern =".tif" )
slodir <- "C:/Users/fyousef/Documents/ESPA_data/ndvi/1-Spring/Slope"
slist <- list.files(slodir, full.names = TRUE, pattern = ".tif") 
outdirs <- "C:/Users/fyousef/Documents/ESPA_data/ndvi/1-Spring/All"
#dir.create(outdirs, showWarning=FALSE, recursive=TRUE)
#tabdir <- "C:/Users/fyousef/Dropbox/UCLA/Results"
#tlist <- list.files(tabdir, full.names = TRUE, pattern = ".csv") 
#tabout <- "C:/Users/fyousef/Dropbox/UCLA/Results/anlz"
#dir.create(tabout, showWarning=FALSE, recursive=TRUE)

## =================== Get Density info ======================
ct=1 
b = vector()
for (x in slist) {
  h <- hist(raster(x), plot=FALSE)
  b[ct] = as.numeric(sum(h$counts))
  #unname(b, force = FALSE)
  ct=ct+1
}

## ==================== Density Plots =========================
i=1
png(paste0(outdirs, "/", "Hist_plots.png"), width=950, height=950, res=120)
par(mfrow=c(4,5), mar=c(4,4,2,2)) #sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.
for (x in flist) {
  nm <- str_sub(names(raster(x)),1,3)
  h <- hist(raster(x), plot=FALSE)
  h$density = h$counts/b[i]*100
  plot(h,freq=FALSE, main=paste(nm), xlim=c(-1,1), xlab = "", ylab="Density (%)")
  abline(v=0,col="red", lty=2)
  i=i+1
}
dev.off()

## ================== Frequency Plots ========================
png(paste0(outdirs, "/", "Frequencyt_plots.png"), width=950, height=950, res=120)
par(mfrow=c(4,5), mar=c(5,5,2,2)) #sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.
for (x in flist) {
  nm <- str_sub(names(raster(x)),1,3)
  hist(raster(x), main=paste(nm), xlim=c(-1,1), xlab = "", ylab="Frequency")
  abline(v=0,col="red", lty=2)
}
dev.off()


## =================== Timeseries Plots ====================
library(ggplot2)
library(ggpmisc) 
library(XLConnect)

# Warning!!!!!
# Change the output filename for each season!
# Also change the slope and Sig(line 81) for different seasons

outdirs <- "C:/Users/fyousef/Dropbox/UCLA/paper/Figures/IEM"
dfspr <- as.data.frame(read.csv("E:/UCLA/ndvi/1_Spring/Tables/All.csv"), header=TRUE)
dfsum <- as.data.frame(read.csv("E:/UCLA/ndvi/2_Summer/Tables/All.csv"), header=TRUE)
slp <- readWorksheetFromFile("E:/UCLA/ndvi/monthly_n_seasonal_trend.xlsx", sheet=1, startRow=2)

slspr <- as.vector(slp$Slope)
slspr <- round(slspr, digits = 7)

slsum <- as.vector(slp$Slope.1)
slsum <- round(slsum, digits = 7)

sig1 <- as.vector(slp$spr_sig)
sig1[is.na(sig1)] <- " "

sig2 <- as.vector(slp$sum_sig)
sig2[is.na(sig2)] <- " "


dfspr$Date <- as.Date(dfspr$Date, format="%m/%d/%Y")
dfsum$Date <- as.Date(dfsum$Date, format="%m/%d/%Y")

formula <- y ~ x

#Spring

g1 <- ggplot(dfspr, aes(x=Date, y=meanNDVI)) + geom_point(size=2,colour = "black") +  
  facet_wrap(~region, ncol=3, scales = "free")  + xlab("Date") + 
  ylab("Mean NDVI") + ylim(0,1) + theme_bw() + 
  theme(strip.text=element_text(size=12, colour="black", face="bold"), 
        plot.title = element_text(hjust = 0.5, size=18, face="bold"), 
        axis.text=element_text(size=10, face="bold", colour="black"), # increae the axis label size
        axis.title = element_text(size = 15)) + 
  labs(title="Mean Spring NDVI (1992-2011)") +
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                  label.x.npc = 0.18, label.y.npc = 0.1, size = 5, fontface="bold") +
  annotate("text", label=paste0("Slope=",slspr), x=as.Date("2005-01-01"), y=0.1, fontface="bold", size=5) +
  annotate("text", label=paste0(sig1), x=as.Date("2010-01-01"), y=0.1, fontface="bold", size=9, color="red")

ggsave(paste0(outdirs, "/","SA-SPR-2018.jpg"), plot=g1, dpi=300, width = 20, height = 15, scale = 0.75)

# Summer

g2 <- ggplot(dfsum, aes(x=Date, y=meanNDVI)) + geom_point(size=2,colour = "black") +  
  facet_wrap(~region, ncol=3, scales = "free")  + xlab("Date") + 
  ylab("Mean NDVI") + ylim(0,1) + theme_bw() + 
  theme(strip.text=element_text(size=12, colour="black", face="bold"), 
        plot.title = element_text(hjust = 0.5, size=18, face="bold"), 
        axis.text=element_text(size=10, face="bold", colour="black"), # increae the axis label size
        axis.title = element_text(size = 15)) + 
  labs(title="Mean Summer NDVI (1992-2011)") +
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                  label.x.npc = 0.18, label.y.npc = 0.1, size = 5, fontface="bold") +
  annotate("text", label=paste0("Slope=",slsum), x=as.Date("2005-01-01"), y=0.1, fontface="bold", size=5) +
  annotate("text", label=paste0(sig2), x=as.Date("2010-01-01"), y=0.1, fontface="bold", size=9, color="red")

ggsave(paste0(outdirs, "/","SA-SUM-2018.jpg"), plot= g2, dpi=300, width = 20, height = 15, scale = 0.75)

## ========================== Bar Plots =====================

library(ggplot2)
library(raster)
library(stringr)
library(plyr)

# This is essentially Figure 6 and 7 barplots
# Change the stuff below to make barplots for spring or summer

sigdir <- "D:/UCLA/ndvi/1_Spring/SigSlp"
flist <- list.files(sigdir, full.names = TRUE, pattern =".tif" )
slodir <- "D:/UCLA/ndvi/1_Spring/Slope"
slist <- list.files(slodir, full.names = TRUE, pattern = ".tif") 
outdirs <- "C:/Users/fyousef/Dropbox/UCLA/paper/Figures/IEM"


ttpix=vector()
ngpix=vector()
pspix=vector()
inspix=vector()
tabnms=vector()


png(paste0(outdirs, "/", "Bar_plot-SP.png"), width=3050, height=3050, res=300)
par(mfrow=c(4,5), mar=c(t=3,r=3,b=2.2,l=2.2))
r2v=1
for (x in flist){
  nm <- str_sub(names(raster(x)),1,3)
  print(paste("This is nm", nm))
  rs <- raster(x) 
  rs2 <- raster(slist[r2v])
  t1 <- count(rs[!is.na(rs)] <0) # count the number of pixels with neg values (rerturns a bolean true/false statement)
  print(paste("This is t1", t1))
  t2 <- count(rs2[!is.na(rs2)] <0)
  print(paste("This is t2", t2))
  pospix <- t1$freq[1]
  print(paste("This is pospix", pospix))
  negpix <- sum(t1$freq)-t1$freq[1]
  print(paste("This is negpix", negpix))
  totsig <- pospix+negpix
  print(paste("This is totsig", totsig))
  totpix <- sum(t2$freq)
  print(paste("This is totpix", totpix))
  totinsg <- totpix- totsig
  print(paste("This is totinsg", totinsg))
  negprc <- negpix/totpix*100
  print(paste("This is negprc", negprc))
  posprc <- pospix/totpix*100
  print(paste("This is posprc", posprc))
  insprc <- totinsg/totpix*100
  print(paste("This is insprc", insprc))
  print(sum(negprc,posprc,insprc))
  fin <- c(round(insprc, digits=1), round(posprc, digits=1), round(negprc, digits=1)) # puting the data together
  nms <- c("Insg", "Pos", "Neg") # defining label names
  clr <- c("gray","green","red") # label colors
  ylim <- c(0, 1.2*max(fin)) # pad a white space above each bar in barplot to insert the percentage (currently 20%)
  xx <- barplot(fin, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim, ylab = "Percent (%)", font.axis = 2, font.lab=2, col=clr) #Constrauct the plot
  text(x = xx, y = fin, label = paste(fin,"%"), pos = 3, cex =1, col = "black", font=2) # add text to the plot
  mtext(text= nm, side=3, line =1, cex=1, font=2)
  mtext(text= paste("n=", totpix), side=3, line =0, cex=0.5, font=2)
  axis(1, at=xx, labels=nms, tick=FALSE, las=1, line=-0.5, font.axis = 2) # Add x-axis labels 
  
  #save table
  tabnms[r2v]=nm
  ttpix[r2v]=totpix
  ngpix[r2v]=negpix
  pspix[r2v]=pospix
  inspix[r2v]=totinsg
  
  
  
  r2v=r2v+1
}
dev.off()

mtab <- data.frame(tabnms,ttpix,ngpix,pspix,inspix)
colnames(mtab) <- c("Site Name", "Total Pix", "Neg Pix", "Pos Pix", "Insg Px")
write.table(mtab, paste0(outdirs, "/", "portions.csv"),row.names = FALSE, append=FALSE, sep=",")

## ============================= Plotting Mean NDVI =============================

meanlist <- list.files("C:/Users/fyousef/Documents/ESPA_data/mean_ndvi_splited/NE/NE4", full.names = TRUE, pattern = ".tif")
sigdir <- "C:/Users/fyousef/Documents/ESPA_data/mean_ndvi_splited/NE"
png(paste0(sigdir, "/", "All_mean_NE4.png"), width=1050, height=1050, res=120)
par(mfrow=c(2,1), mar=c(3,3,2.2,2.2))
for (x in meanlist) {
  nm <- file_path_sans_ext(basename(x))
  rasobj <- raster(x)
  avg <- as.numeric(format(round(cellStats(rasobj, mean),2), nsmall = 2))
  plot(rasobj,  main=paste0(nm, ", mean= ", avg))
}



dev.off()

## =========================== Seasonal Tables =================================

df <- as.data.frame(read.csv(tlist[1], header = TRUE))
df$Date <- as.Date(df$Date, "%m/%d/%Y")

month_name <- c(
  `4` = "April",
  `5` = "May",
  `6` = "June",
  `7` = "July"
)

formula <- y ~ x
ggplot(df, aes(x=Date,y=meanNDVI)) + geom_point(size=2, colour="black")+facet_grid(month ~ region, labeller=labeller(month=as_labeller(month_name))) +
  theme(panel.border=element_blank(), strip.text=element_text(size=12, colour="black"), strip.background=element_rect(colour="white",  fill="white"), axis.text.x=element_text(face="bold", size=10), axis.text.y=element_text(face="bold", size=10), axis.title.x = element_text(face="bold",  size=20), axis.title.y = element_text(face="bold", size=20), strip.text.x = element_text(size=16, face="bold"), strip.text.y = element_text(size=16, face="bold"))+ ylim(0,1.2) + geom_hline(yintercept=c(0.5,1), linetype="dashed") + ylab("EVI") + xlab("Time (year)")+
  stat_poly_eq(aes(label = paste(..rr.label..)), label.x.npc = 0.32, label.y.npc = 0.95, formula = formula, parse = TRUE, size = 4)+
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                  label.x.npc = 0.42, label.y.npc = 0.91, size = 4) +
  ggsave(paste0(tabout, "/","evitt_mon.png"), dpi=120, width = 40, height = 15, scale = 0.75)

## ========================= Linear Regression Summary Stats =====================================
library(plyr)

df <- as.data.frame(read.csv("C:/Users/fyousef/Documents/ESPA_data/evi/2-Summer/Tables/All.csv", header = TRUE))
df$Date <- as.Date(df$Date, "%m/%d/%Y")

season <- "Summer"

#models <- dlply(df,as.quoted(c("region","month")), function(df)lm(meanNDVI ~ Date, data=df)) # ths is for foing month and region
models <- dlply(df,as.quoted(c("region")), function(df)lm(meanNDVI ~ Date, data=df))

mlist <- ldply(models, coef)

#l_ply(models, summary, .print = TRUE)

cnt=1
p_val = vector()
for (x in models){
  p_val[cnt] = (summary(x)$coefficients[2,4])
  cnt=cnt+1
}

mlist$p_val <- as.numeric(round(p_val,digits = 3))
write.table(mlist, paste0("C:/Users/fyousef/Documents/ESPA_data/evi/2-Summer/Tables/","evi_",season,".csv"),row.names = FALSE, append=FALSE, sep=",")

## ========================== Water flow ========================================================


library(reshape2)
library(plyr)
mdf <- read.csv("D:/UCLA/River_flow/KS/Garden_city_monthly.csv")


my_mdf <- melt(mdf, na.rm=TRUE, id=c("Year"))
colnames(my_mdf) <- c("Year", "Month", "Discharge")
my_mdf$Discharge <- as.numeric(my_mdf$Discharge) 
my_models <- dlply(my_mdf,as.quoted(c("Month")), function(my_mdf)lm(Discharge ~ Year, data=my_mdf))

my_list <- ldply(my_models, coef)

cnt=1
p_val = vector()
for (x in my_models){
  p_val[cnt] = (summary(x)$coefficients[2,4])
  cnt=cnt+1
}

my_list$p_val <- as.numeric(round(p_val,digits = 3))

plot(mdf)

## =============================== Canopy Cover Historgrams ===============================
library(raster)


fdw <- list.dirs("D:/UCLA/LULC/Five_states/2011-2001/FDW")
fdw <- fdw[-1]

ndw <- list.dirs("D:/UCLA/LULC/Five_states/2011-2001/NDW")
ndw <- ndw[-1]


p_val=vector()
tbl <- list()
index=1
png("D:/UCLA/LULC/Five_states/Figures/hist_2011s.png", width=1250, height=1050, res=120)
par(mfrow=c(4,5),mar=c(2,2,1,1), font=2)
for (x in 1:17) {
  frs <- raster(fdw[x])
  fdw_mean= cellStats(frs, mean)
  fdw_sd=cellStats(frs,sd)
  fdw_n<- length(frs[values(frs)!="NA"])
  fnm <- toupper(substr(basename(fdw[x]),1,3))
  nrs <- raster(ndw[x])
  ndw_mean= cellStats(nrs, mean)
  ndw_sd=cellStats(nrs,sd)
  ndw_n <- length(frs[values(nrs)!="NA"])
  t=(fdw_mean - ndw_mean)/sqrt((fdw_sd**2/fdw_n)+(ndw_sd**2/ndw_n))
  p_val=1-pt(t,df=(fdw_n + ndw_n)-2, log = FALSE)
  print(paste(fnm, "=",p_val,"F mean=",round(fdw_mean,3),"N mean=",round(ndw_mean,3)))
  tbl[[index]] <- c(fnm, fdw_mean, fdw_sd, ndw_mean, ndw_sd, p_val)
  
  #hf <- hist(frs, breaks=c(-100, -80, -60, -40, -20, -5, 5, 20, 40, 60, 80, 100), plot=F)
  #hf$density = hf$counts/sum(hf$counts)*100
  #hn <- hist(nrs, breaks=c(-100, -80, -60, -40, -20, -5, 5, 20, 40, 60, 80, 100), plot=F)
  #hn$density = hn$counts/sum(hn$counts)*100
  
  #plot(hf, col=rgb(0,1,0,1/4), freq=F, xlab="", main="", xaxt='n', ylim=c(0,100))
  #plot(hn, col=rgb(1,0,0,1/4), freq=F, xlab="", main="", xaxt='n', ann=F, ylim=c(0,100), add=T)
  #axis(side=1, at=seq(-100,100, 20), labels=seq(-100,100,20))
  #abline(v=c(-5,5), col="red", lty=2, lwd=2)
  #legend("topleft", legend=fnm, bty='n', cex=1.5)
  index=index+1
}
dev.off()

df <- as.data.frame(do.call(rbind,tbl))
colnames(df) <- c("Site", "Mean_FDW", "FDW_SD", "Mean_NDW", "NDW_SD", "p-val")
df$Mean_FDW <- round(as.numeric(as.character(df$Mean_FDW)), digits = 2)
df$FDW_SD <- round(as.numeric(as.character(df$FDW_SD)), digits = 2)
df$Mean_NDW <- round(as.numeric(as.character(df$Mean_NDW)), digits = 2)
df$NDW_SD <- round(as.numeric(as.character(df$NDW_SD)), digits = 2)
df$`p-val` <- round(as.numeric(as.character(df$`p-val`)), digits = 3)

write.table(df, file="D:/UCLA/LULC/Five_states/Tables/2001-2011-All-Sites-Stat.csv", sep = ",", row.names=F)

## m mean of the fdw and ndw mean vectors
mean_df <- data.frame("2011", mean(fdw_mean), mean(ndw_mean))
colnames(mean_df) <- c("Year", "Far-downwind", "Near-downwind")
write.table(mean_df, file="D:/UCLA/LULC/Five_states/Tables/2011.csv", sep = ",", row.names=F)

## ============================== CC analysis ====================================
library(foreign)
library(tools)

fdwfls <- list.files("D:/UCLA/GAP/5states/GAP_all/FDW", pattern="*.dbf", full.names=T)
ndwfls <- list.files("D:/UCLA/GAP/5states/GAP_all/NDW", pattern="*.dbf", full.names=T)


fgaptab =list()
ngaptab =list()

index=1
for (x in fdwfls){
  fnm <- file_path_sans_ext(basename(x))
  fDWtab <- read.dbf(x)
  fgaptab[[index]]=c(substr(fnm,1,3), fDWtab)
  index=index+1
}

index=1
for (x in ndwfls){
  fnm <- file_path_sans_ext(basename(x))
  nDWtab <- read.dbf(x)
  ngaptab[[index]]=c(substr(fnm,1,3), nDWtab)
  index=index+1
}

fgap <- do.call(rbind,fgaptab)
ngap <- do.call(rbind,ngaptab)
write.table(fgap, file="D:/UCLA/GAP/Five_states/5states/tables/FDW_mean.csv", sep = ",", row.names=F)
write.table(ngap, file="D:/UCLA/GAP/Five_states/5states/tables/NDW_mean.csv", sep = ",", row.names=F)

# t-test
t.test(fgap[,4], ngap[,4])


png("D:/UCLA/LULC/Five_states/Figures/CC_mean.png",width = 500, height = 700, res=120)
par(mfrow=c(2,1))
hist(fgap$MEAN ,main="FDW", xlab="CC % Change")
hist(ngap$MEAN, main="NDW", xlab="CC % Change")
dev.off()


## ==================================== Gap Analysis ==================================
library(foreign)

fdwfls <- list.files("D:/UCLA/GAP/5states/GAP_mean/FDW", pattern="*.dbf", full.names=T)
ndwfls <- list.files("D:/UCLA/GAP/5states/GAP_mean/NDW", pattern="*.dbf", full.names=T)


fgaptab =list()
ngaptab =list()

index=1
for (x in fdwfls){
  fDWtab <- read.dbf(x)
  fgaptab[[index]]=fDWtab
  index=index+1
}

index=1
for (x in ndwfls){
  nDWtab <- read.dbf(x)
  ngaptab[[index]]=nDWtab
  index=index+1
}

fgap <- do.call(rbind,fgaptab)
fgap$VARIETY[fgap$VARIETY==8] <- 7 # this is just because we dont have 8 in ngap, so I'm changing the only GAP=8 to 7. 
ngap <- do.call(rbind,ngaptab)

write.table(fgap, file="D:/UCLA/LULC/Five_states/mean_can_cover/FDW_mean.csv", sep = ",", row.names=F)
write.table(ngap, file="D:/UCLA/LULC/Five_states/mean_can_cover/NDW_mean.csv", sep = ",", row.names=F)

# chi-square for GAP
tbl <- table(fgap$VARIETY,ngap$VARIETY)

png("D:/UCLA/LULC/Five_states/Figures/CC_mean.png",width = 500, height = 700, res=120)
par(mfrow=c(2,1))
hist(fgap$MEAN ,main="FDW", xlab="CC % Change")
hist(ngap$MEAN, main="NDW", xlab="CC % Change")
dev.off()


## =================================== NDVI NDW n FDW cluster =================================
md <- read.csv("C:/Users/fyousef/Documents/ESPA_data/ndvi/2-Summer/SigSlp/Tbls/Prc_unknown.csv")


cols=vector()
cols=rep(c("blue","red"),17)
png(filename = "C:/Users/fyousef/Documents/ESPA_data/ndvi/2-Summer/All/CLUSTER.png", height = 800, width=1000, res=120)
par(las=3, mar=c(4,6,4,4))
barplot(md$prec_unkn, mpg=c(1.5,1,0.5), names.arg = md$SiteDIR, ylim = c(0,2), col = cols, ylab = "Area of type5 (unknown) clusters \nrelative to site area (%)")
dev.off()
## ================================= 3D Plots =================================================

library(scatterplot3d)

dt <- read.csv("C:/Users/fyousef/Documents/ESPA_data/evi/3d_EVI.csv")

png(filename = "C:/Users/fyousef/Dropbox/UCLA/paper/Figures/Summer_EVI_3d.png", width=700, height=700, res=120)
plt <- scatterplot3d(dt$CENTROID_X,dt$CENTROID_Y, dt$Summer, pch=19,  
                     type="h", xlab="Longitude", ylab="Latitude", zlab="EVI", main="Summer", 
                     angle=100, box=F, mar=c(3,3,3,2.5))
plt.cords <- plt$xyz.convert(dt$CENTROID_X,dt$CENTROID_Y, dt$Summer)
text(plt.cords$x, plt.cords$y, labels=dt$X, pos=3, cex=0.7, font=2)
dev.off()

## =============================== Cluster Barplots ===========================================
library(ggplot2)

dfspr <- read.csv("C:/Users/fyousef/Dropbox/UCLA/Results/cluster/NDVI-spring.csv")
dfspr_neg <- as.vector(t(read.csv("C:/Users/fyousef/Dropbox/UCLA/Results/cluster/NDVI-spring-negs.csv")))
dfsum <- read.csv("C:/Users/fyousef/Dropbox/UCLA/Results/cluster/NDVI-summer.csv")
dfsum_neg <- as.vector(t(read.csv("C:/Users/fyousef/Dropbox/UCLA/Results/cluster/NDVI-summer-negs.csv")))

sp_nm <- colnames(dfspr)[-1]
sp_nm <- gsub("."," ",sp_nm, fixed=T)
su_nm <- colnames(dfsum)[-1]
su_nm <- gsub(".", " ", su_nm, fixed=T)

col=c("gray","green","blue","purple","red")

# For spring
png(filename = "C:/Users/fyousef/Dropbox/UCLA/Results/cluster/spring-clus.png", width=1000, height=350, res=100)
index=1
par(mfrow=c(2,6), mar=c(2,1,1,2))
for (x in 2:11){
  barplot(dfspr[,x], ylim=c(0,100), names.arg=c("RD", "DF", "ER", "UD", "UF"), col=col, ylab="% change")
  text(x=3,y=95, labels=sp_nm[index],font=2)
  text(x=3,y=85, labels=paste0("A=",dfspr_neg[index],"%"))
  index=index+1
}
dev.off()

# For summer
png(filename = "C:/Users/fyousef/Dropbox/UCLA/Results/cluster/summer-clus.png", width=1000, height=1000, res=100)
index=1
par(mfrow=c(6,6), mar=c(2,1,1,2))
for (x in 2:35){
  barplot(dfsum[,x], ylim=c(0,100), names.arg=c("RD", "DF", "ER", "UD", "UF"), col=col, ylab="% change")
  text(x=3,y=95, labels=su_nm[index],font=2)
  text(x=3,y=85, labels=paste0("A=",dfsum_neg[index],"%"))
  index=index+1
}
dev.off()

