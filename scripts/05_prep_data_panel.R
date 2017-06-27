# Preparing data for RATS 

# load libraries
library(tidyverse)
library(raster)

#setwd("/home/paulo/GE712/outputs")
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")

# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-time", type="character", default="seas") # set up time step - monthly, or seasonal 
arg_parser$add_argument("-type", type="character", default="vals") #set up type of data - observations or anomalies 
args <- arg_parser$parse_args()

# Read in climate data, evi and thermal regions
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/precip_sum_%s_%s.csv", args$time, args$type))
temp <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/temp_%s_%s.csv", args$time, args$type))
evi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_%s_%s.csv", args$time, args$type))
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv")

# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
total_filtered = na.omit(total)

# Get season indices
djf_index = grep("DJF", colnames(total_filtered))
mam_index = grep("MAM", colnames(total_filtered))
jja_index = grep("JJA", colnames(total_filtered))
son_index = grep("SON", colnames(total_filtered))

# Subset the seasons!
djf = as.matrix(total_filtered[,djf_index])
mam = as.matrix(total_filtered[,mam_index])
jja = as.matrix(total_filtered[,jja_index])
son = as.matrix(total_filtered[,son_index])

# write out raster files 
seas_raster <- brick(nrows=138, ncols=94, nl=39, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
seas_raster[as.numeric(rownames(djf))] <- djf
writeRaster(seas_raster, file="djf_raster.hdr", format="ENVI", overwrite=TRUE)
seas_raster[as.numeric(rownames(mam))] <- mam
writeRaster(seas_raster, file="mam_raster.hdr", format="ENVI", overwrite=TRUE)
seas_raster[as.numeric(rownames(jja))] <- jja
writeRaster(seas_raster, file="jja_raster.hdr", format="ENVI", overwrite=TRUE)
seas_raster[as.numeric(rownames(son))] <- son
writeRaster(seas_raster, file="son_raster.hdr", format="ENVI", overwrite=TRUE)

# Split variables back into separate df's
total_EVI <- as.data.frame(total_filtered[,1:52])
total_precip <- as.data.frame(total_filtered[,53:104])
total_temp <- as.data.frame(total_filtered[,105:156])
total_regions <- as.data.frame(total_filtered[,157])


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
full_dataset = cbind(tidy_precip, tidy_temp$value, tidy_EVI$value, tidy_regions$value)
colnames(full_dataset) = c("ID", sprintf("%s", args$time), "precip", "temp", "evi", "region")

# Get season indices
djf_index = grep("DJF", full_dataset[,2])
mam_index = grep("MAM", full_dataset[,2])
jja_index = grep("JJA", full_dataset[,2])
son_index = grep("SON", full_dataset[,2])

# Subset the seasons!
djf = full_dataset[djf_index,]
mam = full_dataset[mam_index,]
jja = full_dataset[jja_index,]
son = full_dataset[son_index,]

# Save full dataset and seasonal with variables only so that RATS can read it
write.csv(full_dataset[,3:5], "panel_seasonal_dataset.csv", quote =F, row.names = F)

# write out djf from the different GAEZ thermal zones 
write.csv(djf[which(djf[,6]==1),3:5], sprintf("djf_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,6]==2),3:5], sprintf("djf_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,6]==3),3:5], sprintf("djf_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out mam from the different GAEZ thermal zones 
write.csv(mam[which(mam[,6]==1),3:5], sprintf("mam_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,6]==2),3:5], sprintf("mam_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,6]==3),3:5], sprintf("mam_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out jja from the different GAEZ thermal zones 
write.csv(jja[which(jja[,6]==1),3:5], sprintf("jja_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,6]==2),3:5], sprintf("jja_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,6]==3),3:5], sprintf("jja_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out son from the different GAEZ thermal zones 
write.csv(son[which(son[,6]==1),3:5], sprintf("son_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,6]==2),3:5], sprintf("son_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,6]==3),3:5], sprintf("son_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# Rename var columns for each season so that we can use values from previous seasons
# as a different "variable" (instead of having to deal with RATS lagging). Format ready for RATS

# SUMMER DATA (DJF) starts in SECOND year bc we need spring(SON) and winter (JJA) from previous year
# so we need to remove the first DJF year FROM ALL INDIVIDUALS and the last from SON and JJA

djf_sub = filter(djf, seas != "DJF")
son_sub = filter(son, seas != "SON.12")
jja_sub = filter(jja, seas != "JJA.12")

djf2_zone1 = cbind(djf_sub[which(djf_sub[,6]==1),3:5], son_sub[which(son_sub[,6]==1),3:4], jja_sub[which(jja_sub[,6]==1),3:4])
colnames(djf2_zone1) = c("precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

djf2_zone2 = cbind(djf_sub[which(djf_sub[,6]==2),3:5], son_sub[which(son_sub[,6]==2),3:4], jja_sub[which(jja_sub[,6]==2),3:4])
colnames(djf2_zone2) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

djf2_zone3 = cbind(djf_sub[which(djf_sub[,6]==3),3:5], son_sub[which(son_sub[,6]==3),3:4], jja_sub[which(jja_sub[,6]==3),3:4])
colnames(djf2_zone3) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

# FALL DATA (MAM) starts in SECOND year bc we need summer(DJF) and spring (SON) from previous year
# so we need to remove the first MAM year FROM ALL INDIVIDUALS and the last from DJF and SON
mam_sub = filter(mam, seas != "MAM")
djf_sub = filter(djf, seas != "DJF.12")
son_sub = filter(son, seas != "SON.12")

mam2_zone1 = cbind(mam_sub[which(mam_sub[,6]==1),3:5], djf_sub[which(djf_sub[,6]==1),3:4], son_sub[which(son_sub[,6]==1),3:4])
colnames(mam2_zone1) = c("precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

mam2_zone2 = cbind(mam_sub[which(mam_sub[,6]==2),3:5], djf_sub[which(djf_sub[,6]==2),3:4], son_sub[which(son_sub[,6]==2),3:4])
colnames(mam2_zone2) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

mam2_zone3 = cbind(mam_sub[which(mam_sub[,6]==3),3:5], djf_sub[which(djf_sub[,6]==3),3:4], son_sub[which(son_sub[,6]==3),3:4])
colnames(mam2_zone3) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

# WINTER DATA (JJA) starts in FIRST year bc we have fall(MAM) and summer(DJF) from that year
jja2_zone1 = cbind(jja[which(jja[,6]==1),3:5], mam[which(mam[,6]==1), 3:4], djf[which(djf[,6]==1), 3:4])
colnames(jja2_zone1) = c("precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

jja2_zone2 = cbind(jja[which(jja[,6]==2),3:5], mam[which(mam[,6]==2), 3:4], djf[which(djf[,6]==2), 3:4])
colnames(jja2_zone2) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

jja2_zone3 = cbind(jja[which(jja[,6]==3),3:5], mam[which(mam[,6]==3), 3:4], djf[which(djf[,6]==3), 3:4])
colnames(jja2_zone3) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

# SPRING DATA (SON) starts in FIRST year bc we have winter(JJA) and fall(MAM) from that year
son2_zone1 = cbind(son[which(son[,6]==1),3:5], jja[which(jja[,6]==1), 3:4], mam[which(mam[,6]==1), 3:4])
colnames(son2_zone1) = c("precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

son2_zone2 = cbind(son[which(son[,6]==2),3:5], jja[which(jja[,6]==2), 3:4], mam[which(mam[,6]==2), 3:4])
colnames(son2_zone2) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

son2_zone3 = cbind(son[which(son[,6]==3),3:5], jja[which(jja[,6]==3), 3:4], mam[which(mam[,6]==3), 3:4])
colnames(son2_zone3) = c("precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "precip_lag2", "temp_lag2")

# Save these datasets for use in RATS
write.csv(djf2_zone1, sprintf("djf2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf2_zone2, sprintf("djf2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf2_zone3, sprintf("djf2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(mam2_zone1, sprintf("mam2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam2_zone2, sprintf("mam2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam2_zone3, sprintf("mam2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(jja2_zone1, sprintf("jja2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja2_zone2, sprintf("jja2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja2_zone3, sprintf("jja2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(son2_zone1, sprintf("son2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son2_zone2, sprintf("son2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son2_zone3, sprintf("son2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
