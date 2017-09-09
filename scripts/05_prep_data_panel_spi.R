# Preparing data for RATS 

# load libraries
library(tidyverse)
library(raster)

#setwd("/home/paulo/GE712/outputs")
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")

# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-time", type="character", default="monthly") # set up time step - monthly, or seasonal 
arg_parser$add_argument("-type", type="character", default="vals") #set up type of data - observations or anomalies 
arg_parser$add_argument("-lag", type="character", default="lag1")
args <- arg_parser$parse_args()

# Read in climate data, evi and thermal regions
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/precip_%s_%s.csv", args$time, args$type))
temp <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/temp_%s_%s.csv", args$time, args$type))
evi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_%s_%s.csv", args$time, args$type))
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv")

precip_spi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_%s.csv", args$time, args$type, args$lag))
precip_spi_t <- t(precip_spi)
  
# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
filtered = na.omit(total)
total_filtered <- cbind(filtered[,1:156], precip_spi_t, filtered[,313:468], filtered[,469])

# Subset based on thermal region!
zone1 = as.matrix(total_filtered[which(total_filtered[,469]==1),])
zone2 = as.matrix(total_filtered[which(total_filtered[,469]==2),])
zone3 = as.matrix(total_filtered[which(total_filtered[,469]==3),])

# write out raster files 
seas_raster <- brick(nrows=138, ncols=94, nl=468, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
seas_raster[as.numeric(rownames(zone1))] <- zone1[,-469]
writeRaster(seas_raster, file=sprintf("spi_%s_raster_zone1.hdr", args$lag), format="ENVI", overwrite=TRUE)
seas_raster[as.numeric(rownames(zone2))] <- zone2[,-469]
writeRaster(seas_raster, file=sprintf("spi_%s_raster_zone2.hdr", args$lag), format="ENVI", overwrite=TRUE)
seas_raster[as.numeric(rownames(zone3))] <- zone3[,-469]
writeRaster(seas_raster, file=sprintf("spi_%s_raster_zone3.hdr", args$lag), format="ENVI", overwrite=TRUE)

# Split variables back into separate df's
total_EVI <- as.data.frame(total_filtered[,1:156])
total_precip <- as.data.frame(total_filtered[,157:312])
total_temp <- as.data.frame(total_filtered[,313:468])
total_regions <- as.data.frame(total_filtered[,469])

# Function to tidy each of the dataframes read from the csv files. 
tidy_data = function(df, period_name){
  # Create ID column
  df$ID = seq(1, nrow(df))
  
  # Gather into rows and assign proper column names
  temp_df = gather(df, key="ID", value="value")
  colnames(temp_df) = c("ID", period_name, "value")
  temp_df_sorted =  arrange(temp_df, ID)
}


# Tidy months and seasons
tidy_precip= tidy_data(total_precip, sprintf("%s", args$time))
tidy_temp = tidy_data(total_temp, sprintf("%s", args$time))
tidy_EVI = tidy_data(total_EVI, sprintf("%s", args$time))
tidy_regions = tidy_data(total_regions, sprintf("%s", args$time))

# Merge into months and seasons
full_dataset = cbind(tidy_EVI, tidy_precip$value, tidy_temp$value, tidy_regions$value)
colnames(full_dataset) = c("ID",sprintf("%s", args$time), "evi", "precip", "temp", "region")

# Save full dataset and seasonal with variables only so that RATS can read it
write.csv(full_dataset[,3:5], file=sprintf("panel_spi_%s_dataset.csv", args$lag), quote =F, row.names = F)

# write out djf from the different GAEZ thermal zones 
write.csv(full_dataset[which(full_dataset[,6]==1),3:5], sprintf("zone1_spi_%s.csv", args$lag), quote =F, row.names = F)
write.csv(full_dataset[which(full_dataset[,6]==2),3:5], sprintf("zone2_spi_%s.csv", args$type), quote =F, row.names = F)
write.csv(full_dataset[which(full_dataset[,6]==3),3:5], sprintf("zone3_spi_%s.csv", args$type), quote =F, row.names = F)
