library(rgdal)
library(RColorBrewer)
library(raster)
dpath<- "/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/ArcGIS_files/SA_LPS/saps_cmb/w001001.adf"


# read in global livestock production system classification for 2007 version 3 
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)
plot(r)

setwd("/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/ArcGIS_files/outputs")
writeRaster(r, file="livestock_production_system_SA.hdr", format="ENVI", overwrite=TRUE)


# read in observed global bovine numbers per km squared - from national and subnational census data 
# FAO calls it observed density determined based on most recent available sub-national livestock census 
# data and corresponding administrative boundaries. The data are converted to density (how??) to provide 
# observed data. Then it's disaggregated based on environmental variables to produce the predicted distribution 
# (the dataset I was using before).  

# number per square kilometer 
dpath<- "/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/ArcGIS_files/GLiPHAmaps/global/bovine/obs/glbbvd1o0503m/w001001.adf"

x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)
plot(r)


# the bovine density clipped to South America in ArcGIS by Cholho Song 
dpath<- "/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/ArcGIS_files/flie_glbbvd/glbbvdlatin/w001001.adf"
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)
r[r==-999] <- NA
plot(r)

setwd("/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/ArcGIS_files/outputs")
writeRaster(r, file="livestock_density_observed_SA.hdr", format="ENVI", overwrite=TRUE)

