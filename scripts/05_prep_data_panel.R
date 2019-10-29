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
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals_05.csv")
#colnames(lps_regions) <- "zones"

# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
total_filtered = na.omit(total)

# precip2 <- precip^2
# temp2 <- temp^2

# total <- cbind(evi, precip, temp, precip2, temp2)
# total_filtered = na.omit(total)
# sub_sample <- total_filtered[sample(nrow(total_filtered),100, replace=FALSE, prob=NULL),]
# write.csv(sub_sample, "panel_seasonal_dataset_sample.csv", quote =F, row.names = F)
# 

# Get season indices
djf_index = grep("DJF", colnames(total_filtered))
mam_index = grep("MAM", colnames(total_filtered))
jja_index = grep("JJA", colnames(total_filtered))
son_index = grep("SON", colnames(total_filtered))

# Subset the seasons!
djf_zone1 = as.matrix(total_filtered[which(total_filtered[,157]==1),djf_index])
djf_zone2 = as.matrix(total_filtered[which(total_filtered[,157]==2),djf_index])
djf_zone3 = as.matrix(total_filtered[which(total_filtered[,157]==3),djf_index])

mam_zone1 = as.matrix(total_filtered[which(total_filtered[,157]==1),mam_index])
mam_zone2 = as.matrix(total_filtered[which(total_filtered[,157]==2),mam_index])
mam_zone3 = as.matrix(total_filtered[which(total_filtered[,157]==3),mam_index])

jja_zone1 = as.matrix(total_filtered[which(total_filtered[,157]==1),jja_index])
jja_zone2 = as.matrix(total_filtered[which(total_filtered[,157]==2),jja_index])
jja_zone3 = as.matrix(total_filtered[which(total_filtered[,157]==3),jja_index])

son_zone1 = as.matrix(total_filtered[which(total_filtered[,157]==1),son_index])
son_zone2 = as.matrix(total_filtered[which(total_filtered[,157]==2),son_index])
son_zone3 = as.matrix(total_filtered[which(total_filtered[,157]==3),son_index])

seas_zone_list <- list(djf_zone1, djf_zone2, djf_zone3, mam_zone1, mam_zone2, mam_zone3, 
                       jja_zone1, jja_zone2, jja_zone3, son_zone1, son_zone2, son_zone3)
names(seas_zone_list) <- c("djf_zone1", "djf_zone2","djf_zone3","mam_zone1","mam_zone2","mam_zone3",
                           "jja_zone1","jja_zone2","jja_zone3","son_zone1","son_zone2","son_zone3")
# write out raster files 
for (i in 1:length(seas_zone_list)){
  seas_raster <- brick(nrows=138, ncols=94, nl=39, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                       xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  
  seas_raster[as.numeric(rownames(seas_zone_list[[i]]))] <- seas_zone_list[[i]]
  writeRaster(seas_raster, file=paste(names(seas_zone_list)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
}


# Split variables back into separate df's
total_EVI <- as.data.frame(total_filtered[,1:52])
total_precip <- as.data.frame(total_filtered[,53:104])
total_temp <- as.data.frame(total_filtered[,105:156])

rep.col <- function(x,n){
  matrix(rep(x, each=n), ncol=n, byrow=TRUE)
}
total_regions <- as.data.frame(rep.col(total_filtered[,157], 52))

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

full_dataset_wcell <- cbind(rep(rownames(total_precip), each=52), tidy_precip, tidy_temp$value, tidy_EVI$value, tidy_regions$value)
colnames(full_dataset_wcell) = c("cell", "ID", sprintf("%s", args$time), "precip", "temp", "evi", "region")

# scatter plot of the variables 
full_dataset[grep("DJF", full_dataset[,2]),2] <- "DJF"
full_dataset[grep("MAM", full_dataset[,2]),2] <- "MAM"
full_dataset[grep("JJA", full_dataset[,2]),2] <- "JJA"
full_dataset[grep("SON", full_dataset[,2]),2] <- "SON"

# plot temp versus evi 
p <- ggplot(full_dataset, aes(temp, evi)) + geom_point(aes(colour=seas))
#p + facet_grid(. ~region) + stat_smooth(method="lm", se=FALSE)
p + facet_grid(. ~region) 
p <- ggplot(full_dataset, aes(temp, evi)) + geom_point(aes(colour=region))
p + facet_grid(. ~seas)

# plot precip  versus evi 
p <- ggplot(full_dataset, aes(precip, evi)) + geom_point(aes(colour=seas))
p + facet_grid(. ~region) 
p <- ggplot(full_dataset, aes(precip, evi)) + geom_point(aes(colour=region))
p + facet_grid(. ~seas) 


p <- ggplot(full_dataset, aes(temp, precip)) + geom_point(aes(colour=evi))
p + facet_grid(. ~region)
p <- ggplot(full_dataset, aes(temp, precip)) + geom_point(aes(colour=evi))
p + facet_grid(. ~seas)

p <- ggplot(subset(full_dataset, seas %in% c("SON")), aes(precip, evi)) + geom_point(aes(colour=seas))
p + facet_grid(. ~region)+ stat_smooth(method="lm", se=FALSE)
p <- ggplot(subset(full_dataset, region %in% c(2)), aes(precip, evi)) + geom_point(aes(colour=region))
p + facet_grid(. ~seas) 

# Get season indices
djf_index = grep("DJF", full_dataset_wcell[,3])
mam_index = grep("MAM", full_dataset_wcell[,3])
jja_index = grep("JJA", full_dataset_wcell[,3])
son_index = grep("SON", full_dataset_wcell[,3])

# Subset the seasons!
djf = full_dataset_wcell[djf_index,]
mam = full_dataset_wcell[mam_index,]
jja = full_dataset_wcell[jja_index,]
son = full_dataset_wcell[son_index,]

# Save full dataset and seasonal with variables only so that RATS can read it
write.csv(full_dataset[,3:5], "panel_seasonal_dataset.csv", quote =F, row.names = F)

# write out djf from the different GAEZ thermal zones 
write.csv(djf[which(djf[,7]==1),4:6], sprintf("djf_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,7]==2),4:6], sprintf("djf_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,7]==3),4:6], sprintf("djf_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out mam from the different GAEZ thermal zones 
write.csv(mam[which(mam[,7]==1),4:6], sprintf("mam_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,7]==2),4:6], sprintf("mam_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,7]==3),4:6], sprintf("mam_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out jja from the different GAEZ thermal zones 
write.csv(jja[which(jja[,7]==1),4:6], sprintf("jja_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,7]==2),4:6], sprintf("jja_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,7]==3),4:6], sprintf("jja_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out son from the different GAEZ thermal zones 
write.csv(son[which(son[,7]==1),4:6], sprintf("son_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,7]==2),4:6], sprintf("son_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,7]==3),4:6], sprintf("son_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# Rename var columns for each season so that we can use values from previous seasons
# as a different "variable" (instead of having to deal with RATS lagging). Format ready for RATS

# SUMMER DATA (DJF) starts in SECOND year bc we need spring(SON) and winter (JJA) from previous year
# so we need to remove the first DJF year FROM ALL INDIVIDUALS and the last from SON and JJA

djf_sub = filter(djf, seas != "DJF")
son_sub = filter(son, seas != "SON.12")
jja_sub = filter(jja, seas != "JJA.12")

djf2_zone1 = cbind(djf_sub[which(djf_sub[,7]==1),c(1,4:6)], son_sub[which(son_sub[,7]==1),4:6], jja_sub[which(jja_sub[,7]==1),4:5])
colnames(djf2_zone1) = c("cell","precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "evi_lag1", "precip_lag2", "temp_lag2")

djf2_zone2 = cbind(djf_sub[which(djf_sub[,7]==2),c(1,4:6)], son_sub[which(son_sub[,7]==2),4:6], jja_sub[which(jja_sub[,7]==2),4:5])
colnames(djf2_zone2) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

djf2_zone3 = cbind(djf_sub[which(djf_sub[,7]==3),c(1,4:6)], son_sub[which(son_sub[,7]==3),4:6], jja_sub[which(jja_sub[,7]==3),4:5])
colnames(djf2_zone3) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

# FALL DATA (MAM) starts in SECOND year bc we need summer(DJF) and spring (SON) from previous year
# so we need to remove the first MAM year FROM ALL INDIVIDUALS and the last from DJF and SON
mam_sub = filter(mam, seas != "MAM")
djf_sub = filter(djf, seas != "DJF.12")
son_sub = filter(son, seas != "SON.12")

mam2_zone1 = cbind(mam_sub[which(mam_sub[,7]==1),c(1,4:6)], djf_sub[which(djf_sub[,7]==1),4:6], son_sub[which(son_sub[,7]==1),4:5])
colnames(mam2_zone1) = c("cell","precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

mam2_zone2 = cbind(mam_sub[which(mam_sub[,7]==2),c(1,4:6)], djf_sub[which(djf_sub[,7]==2),4:6], son_sub[which(son_sub[,7]==2),4:5])
colnames(mam2_zone2) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

mam2_zone3 = cbind(mam_sub[which(mam_sub[,7]==3),c(1,4:6)], djf_sub[which(djf_sub[,7]==3),4:6], son_sub[which(son_sub[,7]==3),4:5])
colnames(mam2_zone3) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

# WINTER DATA (JJA) starts in FIRST year bc we have fall(MAM) and summer(DJF) from that year
jja2_zone1 = cbind(jja[which(jja[,7]==1),c(1,4:6)], mam[which(mam[,7]==1), 4:6], djf[which(djf[,7]==1), 4:5])
colnames(jja2_zone1) = c("cell","precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

jja2_zone2 = cbind(jja[which(jja[,7]==2),c(1,4:6)], mam[which(mam[,7]==2), 4:6], djf[which(djf[,7]==2), 4:5])
colnames(jja2_zone2) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

jja2_zone3 = cbind(jja[which(jja[,7]==3),c(1,4:6)], mam[which(mam[,7]==3), 4:6], djf[which(djf[,7]==3), 4:5])
colnames(jja2_zone3) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

# SPRING DATA (SON) starts in FIRST year bc we have winter(JJA) and fall(MAM) from that year
son2_zone1 = cbind(son[which(son[,7]==1),c(1,4:6)], jja[which(jja[,7]==1), 4:6], mam[which(mam[,7]==1), 4:5])
colnames(son2_zone1) = c("cell","precip", "temp", "evi", 
                   "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

son2_zone2 = cbind(son[which(son[,7]==2),c(1,4:6)], jja[which(jja[,7]==2), 4:6], mam[which(mam[,7]==2), 4:5])
colnames(son2_zone2) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

son2_zone3 = cbind(son[which(son[,7]==3),c(1,4:6)], jja[which(jja[,7]==3), 4:6], mam[which(mam[,7]==3), 4:5])
colnames(son2_zone3) = c("cell","precip", "temp", "evi", 
                         "precip_lag1", "temp_lag1", "evi_lag1","precip_lag2", "temp_lag2")

# Save these datasets for use in RATS
write.csv(djf2_zone1[,-1], sprintf("djf2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf2_zone2[,-1], sprintf("djf2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf2_zone3[,-1], sprintf("djf2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(mam2_zone1[,-1], sprintf("mam2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam2_zone2[,-1], sprintf("mam2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam2_zone3[,-1], sprintf("mam2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(jja2_zone1[,-1], sprintf("jja2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja2_zone2[,-1], sprintf("jja2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja2_zone3[,-1], sprintf("jja2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(son2_zone1[,-1], sprintf("son2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son2_zone2[,-1], sprintf("son2_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son2_zone3[,-1], sprintf("son2_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)


djf2_zone1_rshp <- aggregate(.~cell, djf2_zone1, FUN=c)
djf2_zone1_df <- do.call(cbind.data.frame, djf2_zone1_rshp)
djf2_zone2_rshp <- aggregate(.~cell, djf2_zone2, FUN=c)
djf2_zone2_df <- do.call(cbind.data.frame, djf2_zone2_rshp)
djf2_zone3_rshp <- aggregate(.~cell, djf2_zone3, FUN=c)
djf2_zone3_df <- do.call(cbind.data.frame, djf2_zone3_rshp)

mam2_zone1_rshp <- aggregate(.~cell, mam2_zone1, FUN=c)
mam2_zone1_df <- do.call(cbind.data.frame, mam2_zone1_rshp)
mam2_zone2_rshp <- aggregate(.~cell, mam2_zone2, FUN=c)
mam2_zone2_df <- do.call(cbind.data.frame, mam2_zone2_rshp)
mam2_zone3_rshp <- aggregate(.~cell, mam2_zone3, FUN=c)
mam2_zone3_df <- do.call(cbind.data.frame, mam2_zone3_rshp)

jja2_zone1_rshp <- aggregate(.~cell, jja2_zone1, FUN=c)
jja2_zone1_df <- do.call(cbind.data.frame, jja2_zone1_rshp)
jja2_zone2_rshp <- aggregate(.~cell, jja2_zone2, FUN=c)
jja2_zone2_df <- do.call(cbind.data.frame, jja2_zone2_rshp)
jja2_zone3_rshp <- aggregate(.~cell, jja2_zone3, FUN=c)
jja2_zone3_df <- do.call(cbind.data.frame, jja2_zone3_rshp)

son2_zone1_rshp <- aggregate(.~cell, son2_zone1, FUN=c)
son2_zone1_df <- do.call(cbind.data.frame, son2_zone1_rshp)
son2_zone2_rshp <- aggregate(.~cell, son2_zone2, FUN=c)
son2_zone2_df <- do.call(cbind.data.frame, son2_zone2_rshp)
son2_zone3_rshp <- aggregate(.~cell, son2_zone3, FUN=c)
son2_zone3_df <- do.call(cbind.data.frame, son2_zone3_rshp)

seas2_zone_list_1 <- list(djf2_zone1_df, djf2_zone2_df, djf2_zone3_df, mam2_zone1_df, mam2_zone2_df, mam2_zone3_df)
names(seas2_zone_list_1) <- c("djf2_zone1", "djf2_zone2","djf2_zone3","mam2_zone1","mam2_zone2","mam2_zone3")

# write out raster files 
for (i in 1:length(seas2_zone_list_1)){
  seas2_raster <- brick(nrows=138, ncols=94, nl=96, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                       xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  
  seas2_raster[as.numeric(as.character(seas2_zone_list_1[[i]][,1]))] <- as.matrix(seas2_zone_list_1[[i]][,-1])
  writeRaster(seas2_raster, file=paste(names(seas2_zone_list_1)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
}

seas2_zone_list_2 <- list(jja2_zone1_df, jja2_zone2_df, jja2_zone3_df, son2_zone1_df, son2_zone2_df, son2_zone3_df)
names(seas2_zone_list_2) <- c("jja2_zone1","jja2_zone2","jja2_zone3","son2_zone1","son2_zone2","son2_zone3")

# write out raster files 
for (i in 1:length(seas2_zone_list_2)){
  seas2_raster <- brick(nrows=138, ncols=94, nl=104, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  
  seas2_raster[as.numeric(as.character(seas2_zone_list_2[[i]][,1]))] <- as.matrix(seas2_zone_list_2[[i]][,-1])
  writeRaster(seas2_raster, file=paste(names(seas2_zone_list_2)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
}
