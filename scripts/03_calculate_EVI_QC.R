# This script read and filter EVI timeseries based on QA/QC from MOD13C2 tile stack
# Updated: April 2017 by Radost Stanimirova 

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
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

path_MOD13C2 <- "/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/" # path to your MOD13C2 files

# read in data ----------------------------

# read EVI timeseries 
GetEVI <- function(MOD13C2_file_path, as.int=T){
  fun <- function(x) {x[x==-3000] <- NA; return(x)}
  
  SDS_EVI <- MOD13C2_file_path
  evi <- raster(SDS_EVI) # read in the red SDS as a raster
  NAvalue(evi) <- -3000
  evi <- calc(evi, fun)
  return(evi)
  
}

# set up parallel analysis 
clusterExport(cl, c("GetEVI"))

MOD13C2_tile_files <- Sys.glob(file.path(path_MOD13C2, paste("MOD13C2", "*", '006',"*",'SA.clip', "tif", sep=".")))
# calculate EVI
evi_list <- parLapply(cl, MOD13C2_tile_files, GetEVI)
evi_stack <- stack(unlist(evi_list)) # unlist and convert to a single RasterStack

evi_final <- evi_stack/10000
evi_vals <- values(evi_final)

evi_vals[which(evi_vals<0 & !is.na(evi_vals))] <- NA

# read EVI QA/QC timeseries 
MOD13C2_tile_files <- Sys.glob(file.path(path_MOD13C2, paste("MOD13C2", "*", '006',"*", 'SA.QA.clip', "tif", sep=".")))

GetQC <- function(MOD13C2_file_path, as.int=T){
  fun <- function(x) {x[x==65535] <- NA; return(x)}
  
  SDS_QC <- MOD13C2_file_path
  qc <- raster(SDS_QC) # read in the red SDS as a raster
  NAvalue(qc) <- 65535
  qc <- calc(qc, fun)
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

# create a raster file from the filtered EVI timeseries 

south_am <- brick(nrows=1380, ncols=940, nl=156, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
south_am <- setValues(south_am, evi_vals)

setwd("/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/")
writeRaster(south_am, NAflag=-9999, file="filtered_EVI.hdr", format="ENVI", overwrite=TRUE)
#writeRaster(south_am, file="filtered_EVI.tif", format="GTiff", options=c("COMPRESS=PACKBITS"))


