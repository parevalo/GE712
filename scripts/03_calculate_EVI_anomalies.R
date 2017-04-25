# read in required libraries ------------------------
library(sp)
library(rgdal)
#library(tiff)
library(raster)
library(lubridate)
library(maptools)

# set up to run in parallel 
library(parallel)
cl <- makeCluster(8)
clusterEvalQ(cl, {library(raster)})

# define functions --------------------------
# make a matrix of all the z score values from a raster stack
get_raster_vals <- function(data){
  
  z_values <- matrix(NA, nrow=259200,ncol=dim(data)[3])
  for (i in 1:dim(data)[3]) {
    sub_values1 <- getValues(data[[i]])
    
    if(i==1){
      z_values <-sub_values1
      
    }else{
      z_values <- cbind(z_values, sub_values1)
      
    }
  }
  return(z_values)
  
}


# assign in variables ------------------------
start.year <- "2000"
end.year <- "2014"

data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Panama", "Costa Rica", "Nicaragua")
NorthAm <- c("Canada", "Mexico", "United States")

sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]
na <- wrld_simpl[which(wrld_simpl@data$NAME %in% NorthAm),]

# read in data ----------------------------

pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture_2000_sub.tif")
pasture_2000_sub[pasture_2000_sub<0.6] <- NA

path_MOD13C2 <- "/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/" # path to your MOD13C2 files
r <- raster(nrows=600, ncols=700)
rowcol <- rowColFromCell(r, c(1:420000))

GetEVI <- function(MOD13C2_file_path, as.int=T){
  fun <- function(x) {x[x==32767] <- NA; return(x)}
  
  SDS_EVI <- MOD13C2_file_path
  evi <- raster(SDS_EVI) # read in the red SDS as a raster
  NAvalue(evi) <- 32767
  evi <- calc(evi, fun)
  return(evi)
  
}

# set up parallel analysis 
clusterExport(cl, c("GetEVI"))

MOD13C2_tile_files <- Sys.glob(file.path(path_MOD13C2, paste("MOD13C2", "*", '006','*_EVI', "tif", sep=".")))
#MOD13C2_tile_files <- Sys.glob(file.path(path_MOD13C2, paste("MOD13C2", "*", '006',"*", "hdf", sep=".")))

# calculate EVI
evi_list <- parLapply(cl, MOD13C2_tile_files, GetEVI)
evi_stack <- stack(unlist(evi_list)) # unlist and convert to a single RasterStack
#r2 <- mask(evi_stack, LC_Amazon)
evi_final <- evi_stack/10000
evi_vals <- values(evi_final)

evi_vals[which(evi_vals<0 & !is.na(evi_vals))] <- NA

path_MOD13C2 <- "/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/"
MOD13C2_tile_files <- Sys.glob(file.path(path_MOD13C2, paste("MOD13C2", "*", '006','*_Quality', "tif", sep=".")))

GetQC <- function(MOD13C2_file_path, as.int=T){
  
  SDS_QC <- MOD13C2_file_path
  qc <- raster(SDS_QC) # read in the red SDS as a raster
  return(qc)
  
}

q_list <- parLapply(cl, MOD13C2_tile_files, GetQC)
q_stack <- stack(unlist(q_list)) # unlist and convert to a single RasterStack
qc <- values(q_stack)


qcv <- as.vector(as.matrix(qc))

qc_00_01 <- rep(NA, length(qc))
qc_02_05 <- rep(NA, length(qc))
qc_06_07 <- rep(NA, length(qc))
qc_08 <- rep(NA, length(qc))
qc_10 <- rep(NA, length(qc))
qc_fin <- rep(NA, length(qc))
#BinToDec <- function(x) sum(2^(which(unlist(strsplit(as.character(x), "")) ==1)-1))

for (t in 1:length(qcv)){
  
  quality_bit <- as.numeric(intToBits(qcv[t]))
  # bits 0 & 1
  qc_00_01[t] <- 1.*quality_bit[1] + 2.*quality_bit[2]
  # bits 2-5 VI usefulness
  qc_02_05[t] <- 1.*quality_bit[3] + 2.*quality_bit[4]+4.*quality_bit[5]+8.*quality_bit[6]
  # bits 6-7 aerosol
  qc_06_07[t] <- 1.*quality_bit[7] + 2.*quality_bit[8]
  # bit 8 Adjacent cloud detected
  qc_08[t] <- 1.*quality_bit[9]
  # bit 10 Mixed clouds
  qc_10[t] <- 1.*quality_bit[11] 
  
}

qc_fin[(qc_00_01==1 | qc_00_01==0) & (qc_02_05>=0 & qc_02_05<=11) & (qc_08==0) & (qc_10==0) & (qc_06_07>=1 & qc_06_07<=2)]=1
#qc_index <- qc_fin==0
#evi_vals[which(qc_fin==0)]<-NA
evi_vals[is.na(qc_fin)]<-NA

south_am <- brick(nrows=254, ncols=319, nl=30, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                xmn=-109.5, xmx=-29.75, ymn=-50, ymx=13.5)
south_am <- setValues(south_am, evi_vals)

sa_pasture_sub <- mask(south_am, pasture_2000_sub)

#sa_pasture_sub <- crop(south_am , extent(pasture_2000_sub))
index <- rep(1:10, each=3)

sa_year_mean <- stackApply(sa_pasture_sub, index, mean, na.rm=T)
sa_year_sd <- stackApply(sa_pasture_sub, index, sd, na.rm=T)
sa_year_cv <- stackApply(sa_pasture_sub, index, cv, na.rm=T)

for (i in 1:10){
  out_stack_mean <- calc(dropLayer(sa_year_mean, i), mean)
  out_stack_sd <- calc(dropLayer(sa_year_mean, i), sd)
  out_stack_year_mean <- sa_year_mean[[i]]
  
  if(i==1){
    evi_dry_season_mean <-out_stack_mean 
    #assign(paste("stack", GetYear(files[i]), sep="_"), out_stack)
    evi_dry_season_sd <-out_stack_sd
    evi_year_mean <-out_stack_year_mean
    
  }else{
    evi_dry_season_mean  <- append(evi_dry_season_mean, out_stack_mean)
    evi_dry_season_sd  <- append(evi_dry_season_sd, out_stack_sd)
    evi_year_mean  <- append(evi_year_mean, out_stack_year_mean)
    
  }
}

evi_dry_season_mean <- stack(evi_dry_season_mean)
evi_dry_season_sd <- stack(evi_dry_season_sd)
evi_year_mean <- stack(evi_year_mean)

evi_anomaly <- (evi_year_mean - evi_dry_season_mean)/evi_dry_season_sd
names(evi_anomaly) <- paste0("Y", 2002:2011)
spplot(evi_anomaly, bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(evi_anomaly, bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)

evi_dry_season_anomaly <- get_raster_vals(evi_anomaly)
write.csv(evi_dry_season_anomaly, file="/projectnb/modislc/users/rkstan/GE529/GE529_Jian_Lab1_Codes/evi_dry_season_anomaly_SA.csv", quote=FALSE, row.names=FALSE)


evi_cv <- cv(evi_dry_season_mean, na.rm=T)

# plot -------------------------------------------------

plot(pasture_2000_sub, col=brewer.pal(4, "YlOrBr"), xlim=c(xmin(pasture_2000_sub), xmax(pasture_2000_sub)),ylim=c(ymin(pasture_2000_sub), ymax(pasture_2000_sub)),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(pasture_2000_sub, col=brewer.pal(4, "Reds"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Pasture 2000", side=4, line=2.9, cex=1.25))

plot(evi_anomaly[[8]],col=brewer.pal(6, "RdYlGn"),  xlim=c(xmin(evi_anomaly[[9]]), xmax(evi_anomaly[[9]])),ylim=c(ymin(evi_anomaly[[9]]), ymax(evi_anomaly[[9]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(evi_anomaly[[8]]),at=seq(xmin(evi_anomaly[[8]]),xmax(evi_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(evi_anomaly[[8]]),at=seq(ymin(evi_anomaly[[8]]),ymax(evi_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(evi_anomaly[[8]], col=brewer.pal(6, "RdYlGn"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Anomaly", side=4, line=2.9, cex=1.25))

mean_all <- mean(sa_year_mean, na.rm=T)
plot(mean_all,col=brewer.pal(8, "Greens"),  xlim=c(xmin(mean_all), xmax(mean_all)),ylim=c(ymin(mean_all), ymax(mean_all)),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(mean_all, col=brewer.pal(8, "Greens"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Mean", side=4, line=2.9, cex=1.25))

cv_all <- cv(sa_year_mean, na.rm=T)
plot(cv_all,col=brewer.pal(8, "OrRd"),  xlim=c(xmin(cv_all), xmax(cv_all)),ylim=c(ymin(cv_all), ymax(cv_all)),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(cv_all, col=brewer.pal(8, "OrRd"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Coefficient of Variation", side=4, line=2.9, cex=1.25))



evi_dry_season_mean_2005 <- calc(dropLayer(sa_year_mean, 10), mean)
evi_dry_season_sd_2005 <- calc(dropLayer(sa_year_mean, 10), sd)
evi_2005_mean <- sa_year_mean[[10]]
evi_2005_anomaly <- (evi_2005_mean - evi_dry_season_mean_2005)/evi_dry_season_sd_2005


# plot(sa_year_sd[[8]],col=brewer.pal(8, "OrRd"),  xlim=c(xmin(sa_year_sd[[8]]), xmax(sa_year_sd[[8]])),ylim=c(ymin(sa_year_sd[[8]]), ymax(sa_year_sd[[8]])),
#      cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
# plot(sa, add=T,lty=1,lwd=0.5)
# plot(sa_year_sd[[8]], col=brewer.pal(8, "OrRd"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Standard Deviation", side=4, line=2.9, cex=1.25))

# Extras 
# -----------

# evi_dry_season_mean_2005 <- calc(dropLayer(sa_year_mean, 4), mean)
# evi_dry_season_sd_2005 <- calc(dropLayer(sa_year_mean, 4), sd)
# 
# evi_dry_season_mean_2008 <- calc(dropLayer(sa_year_mean, 7), mean)
# evi_dry_season_sd_2008 <- calc(dropLayer(sa_year_mean, 7), sd)
# 
# evi_dry_season_mean_2010 <- calc(dropLayer(sa_year_mean, 9), mean)
# evi_dry_season_sd_2010 <- calc(dropLayer(sa_year_mean, 9), sd)

# evi_2005_mean <- sa_year_mean[[4]]
# evi_2008_mean <- sa_year_mean[[7]]
# evi_2010_mean <- sa_year_mean[[9]]

# evi_2005_anomaly <- (evi_2005_mean - evi_dry_season_mean_2005)/evi_dry_season_sd_2005
# evi_2008_anomaly <- (evi_2008_mean - evi_dry_season_mean_2008)/evi_dry_season_sd_2008
# evi_2010_anomaly <- (evi_2010_mean - evi_dry_season_mean_2010)/evi_dry_season_sd_2010

# setwd("/projectnb/modislc/users/rkstan/GE529/GE529_Jian_Lab1_Codes/")
# pdf("evi_anomalies_SA.pdf", height=9, width=9)
# par(mfrow=c(1,3),pty="s", oma=c(4,4,4,4), mar=c(1,1,1,1))
# plot(evi_2005_anomaly, col=brewer.pal(7, "RdYlGn"),colNA="gray",
#      #legend.width=1, legend.shrink=1,
#      #legend.args=list("0.25 degree evi. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50),
#      legend=FALSE
# )
# plot(sa, add=T)
# legend("topright", legend=c("2005"), bty='n', cex=1.5)
# 
# plot(evi_2008_anomaly, col=brewer.pal(7, "RdYlGn"), legend=FALSE, colNA="gray",
#      #legend.width=1, legend.shrink=1,
#      #legend.args=list("0.25 degree evi. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50)
# )
# plot(sa, add=T)
# legend("topright", legend=c("2008"), bty='n', cex=1.5)
# 
# plot(evi_2010_anomaly, col=brewer.pal(7, "RdYlGn"),colNA="gray",
#      legend.width=1, legend.shrink=1, 
#      #legend.args=list("0.25 degree evi. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50)
# )
# plot(sa, add=T)
# legend("topright", legend=c("2010"), bty='n', cex=1.5)
# dev.off()
