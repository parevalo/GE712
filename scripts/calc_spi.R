###############################################################
# Claculate SPEI 2,3,4,5, and 6 months

# script last edited by Radost Stanimirova Summer 2017
###############################################################

# set working directory ----------------

setwd("/projectnb/modislc/users/rkstan/GE712/data/TRMM")

# load libraries ------------------
library(ncdf4)
library(RColorBrewer)

library(sp)
library(raster)
library(rgdal)
library(gtools)
library(maptools)
library(SPEI)

# set variables ----------------------------
m <- rep(1:12, 15)
yr <- rep(1:15, each=12)

data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# set functions ----------------------

# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-time", type="character", default="monthly") # set up time step - monthly, or seasonal 
arg_parser$add_argument("-type", type="character", default="vals") #set up type of data - observations or anomalies 
args <- arg_parser$parse_args()

# Read in climate data, evi and thermal regions
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/precip_%s_%s.csv", args$time, args$type))
temp <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/temp_%s_%s.csv", args$time, args$type))
evi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_%s_%s.csv", args$time, args$type))
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv")


# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
total_filtered = na.omit(total)

precip_clean <- total_filtered[,157:312]
precip_t <- t(precip_clean)

# calculate SPI with precipitation raw values 

precip_lag1 <- spi(precip_t, 1, na.rm=T)
write.csv(precip_lag1$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag1.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
precip_lag1_t <- t(precip_lag1$fitted)

# write out raster files 
seas_raster <- brick(nrows=138, ncols=94, nl=156, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
seas_raster[as.numeric(rownames(precip_clean))] <- precip_lag1_t

precip_lag2 <- spi(precip_t, 2)
write.csv(precip_lag2$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag2.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
precip_lag3 <- spi(precip_t, 3)
write.csv(precip_lag3$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag3.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
precip_lag4 <- spi(precip_t, 4)
write.csv(precip_lag4$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag4.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
precip_lag5 <- spi(precip_t, 5)
write.csv(precip_lag5$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag5.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
precip_lag6 <- spi(precip_t, 6)
write.csv(precip_lag6$fitted, file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_lag6.csv", args$time, args$type), quote=FALSE, row.names=FALSE)
