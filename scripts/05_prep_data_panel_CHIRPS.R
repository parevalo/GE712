# Preparing data for RATS 

# load libraries
library(tidyverse)
library(raster)
library(plyr)
library(dplyr)
library(data.table)

#setwd("/home/paulo/GE712/outputs")
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")

# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-time", type="character", default="seas") # set up time step - monthly, or seasonal 
arg_parser$add_argument("-type", type="character", default="vals") #set up type of data - observations or anomalies 
args <- arg_parser$parse_args()

# Read in climate data, evi and thermal regions
#precip <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
#precip <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_%s_%s_005.csv", args$time, args$type))
precip <- read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_sum_seas_vals.csv")
evi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_%s_%s_005.csv", args$time, args$type))
lps_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals_zones.csv")
lps_regions_production <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals.csv")
colnames(lps_regions) <- "zones"

# Merge and remove rows with at least one NA
total <- cbind(evi, precip, lps_regions_production, lps_regions)
total_filtered = na.omit(total)
total_filtered_sample <- ddply(total_filtered, .(zones), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),1:105])


# # Get season indices
# djf_index = grep("DJF", colnames(total_filtered_sample))
# mam_index = grep("MAM", colnames(total_filtered_sample))
# jja_index = grep("JJA", colnames(total_filtered_sample))
# son_index = grep("SON", colnames(total_filtered_sample))
# 
# # Subset the seasons!
# djf_zone1 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==1),djf_index])
# djf_zone2 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==2),djf_index])
# djf_zone3 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==3),djf_index])
# djf_zone4 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==4),djf_index])
# 
# mam_zone1 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==1),mam_index])
# mam_zone2 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==2),mam_index])
# mam_zone3 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==3),mam_index])
# mam_zone4 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==4),mam_index])
# 
# jja_zone1 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==1),jja_index])
# jja_zone2 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==2),jja_index])
# jja_zone3 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==3),jja_index])
# jja_zone4 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==4),jja_index])
# 
# son_zone1 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==1),son_index])
# son_zone2 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==2),son_index])
# son_zone3 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==3),son_index])
# son_zone4 = as.matrix(total_filtered_sample[which(total_filtered_sample[,105]==4),son_index])
# 
# seas_zone_list <- list(djf_zone1, djf_zone2, djf_zone3,  djf_zone4,mam_zone1, mam_zone2, mam_zone3, mam_zone4,
#                        jja_zone1, jja_zone2, jja_zone3, jja_zone4,son_zone1, son_zone2, son_zone3, son_zone4)
# names(seas_zone_list) <- c("djf_zone1", "djf_zone2","djf_zone3","djf_zone4","mam_zone1","mam_zone2","mam_zone3","mam_zone4",
#                            "jja_zone1","jja_zone2","jja_zone3","jja_zone4","son_zone1","son_zone2","son_zone3","son_zone4")
# # # write out raster files 
# # for (i in 1:length(seas_zone_list)){
#  seas_raster <- brick(nrows=138, ncols=94, nl=39, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#                         xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
# #   setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
# #   
# #   seas_raster[as.numeric(rownames(seas_zone_list[[i]]))] <- seas_zone_list[[i]]
# #   writeRaster(seas_raster, file=paste(names(seas_zone_list)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
# # }


# Split variables back into separate df's
total_EVI <- as.data.frame(total_filtered[,1:52])
total_precip <- as.data.frame(total_filtered[,53:104])

rep.col <- function(x,n){
  matrix(rep(x, each=n), ncol=n, byrow=TRUE)
}
total_regions_production <- as.data.frame(rep.col(total_filtered[,105], 52))
total_regions <- as.data.frame(rep.col(total_filtered[,106], 52))

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
tidy_EVI = tidy_data(total_EVI, sprintf("%s", args$time))
tidy_regions_production = tidy_data(total_regions_production, sprintf("%s", args$time))
tidy_regions = tidy_data(total_regions, sprintf("%s", args$time))

# Merge into months and seasons
full_dataset = cbind(tidy_precip, tidy_EVI$value, tidy_regions_production$value, tidy_regions$value)
colnames(full_dataset) = c("ID", sprintf("%s", args$time), "precip", "evi", "production","region")

full_dataset_wcell <- cbind(rep(rownames(total_precip), each=52), tidy_precip, tidy_EVI$value, tidy_regions_production$value,tidy_regions$value)
colnames(full_dataset_wcell) = c("cell", "ID", sprintf("%s", args$time), "precip","evi", "production","region")

# precip_nonlagged = filter(full_dataset_wcell, seas != "DJF")
# precip_lagged = filter(full_dataset_wcell, seas != "SON.12")

#full_dataset_final <- cbind(precip_nonlagged, precip_lagged$precip)

# scatter plot of the variables 
full_dataset[grep("DJF", full_dataset[,2]),2] <- "DJF"
full_dataset[grep("MAM", full_dataset[,2]),2] <- "MAM"
full_dataset[grep("JJA", full_dataset[,2]),2] <- "JJA"
full_dataset[grep("SON", full_dataset[,2]),2] <- "SON"

sample_dataset <- ddply(full_dataset, .(production), function(x) x[sample(nrow(x),1000, replace=FALSE, prob=NULL),2:6])

# # plot precip  versus evi 
p <- ggplot(sample_dataset, aes(precip, evi)) + geom_point(aes(colour=seas))
p + facet_grid(. ~production) 
p <- ggplot(sample_dataset, aes(precip, evi)) + geom_point(aes(colour=region))
p + facet_grid(. ~seas) + stat_smooth(method="lm", se=FALSE)


p <- ggplot(subset(full_dataset, seas %in% c("SON")), aes(precip, evi)) + geom_point(aes(colour=seas))
p + facet_grid(. ~region)+ stat_smooth(method="lm", se=FALSE)
p <- ggplot(subset(sample_dataset, production %in% c(1)), aes(precip, evi)) + geom_point(aes(colour=region))
p + facet_grid(. ~seas)+ stat_smooth(method="lm", se=FALSE)

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

djf_rshp <- aggregate(.~cell, djf, FUN=c)
djf_df <- do.call(cbind.data.frame, djf_rshp)
mam_rshp <- aggregate(.~cell, mam, FUN=c)
mam_df <- do.call(cbind.data.frame, mam_rshp)
jja_rshp <- aggregate(.~cell, jja, FUN=c)
jja_df <- do.call(cbind.data.frame, jja_rshp)
son_rshp <- aggregate(.~cell, son, FUN=c)
son_df <- do.call(cbind.data.frame, son_rshp)

# seas_list <- list(djf_df,mam_df, jja_df, son_df)
# 
# for (i in 1:length(seas_list)){
# seas2_raster <- brick(nrows=1380, ncols=940, nl=52, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#                       xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
# setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
# 
# seas2_raster[as.numeric(as.character(seas_list[[i]][,1]))] <- as.matrix(seas_list[[i]][,-c(1:27)])
# writeRaster(seas2_raster, file=paste("CHIRPS_seasons", ".hdr", sep=""), format="ENVI", overwrite=TRUE)
# }

# djf = setDT(djf)[,if(!all(precip == 0)) .SD, cell]
# djf$num <- rep(1:13,67612)
# mam = setDT(mam)[,if(!all(precip == 0)) .SD, cell]
# mam$num <- rep(1:13, 67861)
# jja = setDT(jja)[,if(!all(precip == 0)) .SD, cell]
# jja$num <- rep(1:13, 67846)
# son = setDT(son)[,if(!all(precip == 0)) .SD, cell]
# son$num <- rep(1:13, 67978)
# 
# seas_merged1 <- merge(djf, mam, by=c("cell","num"))
# seas_merged2 <- merge(seas_merged1, jja, by=c("cell","num"))
# seas_merged3 <- merge(seas_merged2, son, by=c("cell","num"))
# 
# djf <- seas_merged3[,c(1,3:7)]
# mam <- seas_merged3[,c(1,8:12)]
# jja <- seas_merged3[,c(1,13:17)]
# son <- seas_merged3[,c(1,18:22)]

# Save full dataset and seasonal with variables only so that RATS can read it
write.csv(full_dataset[,3:4], "CHIRPS_panel_seasonal_dataset.csv", quote =F, row.names = F)

# write out djf from the different GAEZ thermal zones 
write.csv(djf[which(djf[,7]==1),4:5], sprintf("CHIRPS_djf_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,7]==2),4:5], sprintf("CHIRPS_djf_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,7]==3),4:5], sprintf("CHIRPS_djf_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(djf[which(djf[,7]==4),4:5], sprintf("CHIRPS_djf_zone4_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out mam from the different GAEZ thermal zones 
write.csv(mam[which(mam[,7]==1),4:5], sprintf("CHIRPS_mam_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,7]==2),4:5], sprintf("CHIRPS_mam_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,7]==3),4:5], sprintf("CHIRPS_mam_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(mam[which(mam[,7]==4),4:5], sprintf("CHIRPS_mam_zone4_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out jja from the different GAEZ thermal zones 
write.csv(jja[which(jja[,7]==1),4:5], sprintf("CHIRPS_jja_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,7]==2),4:5], sprintf("CHIRPS_jja_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,7]==3),4:5], sprintf("CHIRPS_jja_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(jja[which(jja[,7]==4),4:5], sprintf("CHIRPS_jja_zone4_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# write out son from the different GAEZ thermal zones 
write.csv(son[which(son[,7]==1),4:5], sprintf("CHIRPS_son_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,7]==2),4:5], sprintf("CHIRPS_son_zone2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,7]==3),4:5], sprintf("CHIRPS_son_zone3_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(son[which(son[,7]==4),4:5], sprintf("CHIRPS_son_zone4_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

# Rename var columns for each season so that we can use values from previous seasons
# as a different "variable" (instead of having to deal with RATS lagging). Format ready for RATS

# SUMMER DATA (DJF) starts in SECOND year bc we need spring(SON) and winter (JJA) from previous year
# so we need to remove the first DJF year FROM ALL INDIVIDUALS and the last from SON and JJA

djf_sub = filter(djf, seas != "DJF")
son_sub = filter(son, seas != "SON.12")
jja_sub = filter(jja, seas != "JJA.12")

djf2 = cbind(djf_sub[,c(1,4:6)], son_sub[,4:5], jja_sub[,4])
colnames(djf2) = c("cell","precip", "evi",  "production",
                         "precip_lag1", "evi_lag1", "precip_lag2")


djf2_zone1 = cbind(djf_sub[which(djf_sub[,7]==1),c(1,4:6)], son_sub[which(son_sub[,7]==1),4:5], jja_sub[which(jja_sub[,7]==1),4])
colnames(djf2_zone1) = c("cell","precip", "evi",  "production",
                         "precip_lag1", "evi_lag1", "precip_lag2")

djf2_zone2 = cbind(djf_sub[which(djf_sub[,7]==2),c(1,4:6)], son_sub[which(son_sub[,7]==2),4:5], jja_sub[which(jja_sub[,7]==2),4])
colnames(djf2_zone2) = c("cell","precip",  "evi",  "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

djf2_zone3 = cbind(djf_sub[which(djf_sub[,7]==3),c(1,4:6)], son_sub[which(son_sub[,7]==3),4:5], jja_sub[which(jja_sub[,7]==3),4])
colnames(djf2_zone3) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

djf2_zone4 = cbind(djf_sub[which(djf_sub[,7]==4),c(1,4:6)], son_sub[which(son_sub[,7]==4),4:5], jja_sub[which(jja_sub[,7]==4),4])
colnames(djf2_zone4) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

# FALL DATA (MAM) starts in SECOND year bc we need summer(DJF) and spring (SON) from previous year
# so we need to remove the first MAM year FROM ALL INDIVIDUALS and the last from DJF and SON
mam_sub = filter(mam, seas != "MAM")
djf_sub = filter(djf, seas != "DJF.12")
son_sub = filter(son, seas != "SON.12")

mam2 = cbind(mam_sub[,c(1,4:6)], djf_sub[,4:5], son_sub[,4])
colnames(mam2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")


mam2_zone1 = cbind(mam_sub[which(mam_sub[,7]==1),c(1,4:6)], djf_sub[which(djf_sub[,7]==1),4:5], son_sub[which(son_sub[,7]==1),4])
colnames(mam2_zone1) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

mam2_zone2 = cbind(mam_sub[which(mam_sub[,7]==2),c(1,4:6)], djf_sub[which(djf_sub[,7]==2),4:5], son_sub[which(son_sub[,7]==2),4])
colnames(mam2_zone2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

mam2_zone3 = cbind(mam_sub[which(mam_sub[,7]==3),c(1,4:6)], djf_sub[which(djf_sub[,7]==3),4:5], son_sub[which(son_sub[,7]==3),4])
colnames(mam2_zone3) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

mam2_zone4 = cbind(mam_sub[which(mam_sub[,7]==4),c(1,4:6)], djf_sub[which(djf_sub[,7]==4),4:5], son_sub[which(son_sub[,7]==4),4])
colnames(mam2_zone4) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

# WINTER DATA (JJA) starts in FIRST year bc we have fall(MAM) and summer(DJF) from that year
jja2 = cbind(jja[,c(1,4:6)], mam[, 4:5], djf[, 4])
colnames(jja2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

jja2_zone1 = cbind(jja[which(jja[,7]==1),c(1,4:6)], mam[which(mam[,7]==1), 4:5], djf[which(djf[,7]==1), 4])
colnames(jja2_zone1) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

jja2_zone2 = cbind(jja[which(jja[,7]==2),c(1,4:6)], mam[which(mam[,7]==2), 4:5], djf[which(djf[,7]==2), 4])
colnames(jja2_zone2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

jja2_zone3 = cbind(jja[which(jja[,7]==3),c(1,4:6)], mam[which(mam[,7]==3), 4:5], djf[which(djf[,7]==3), 4])
colnames(jja2_zone3) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

jja2_zone4 = cbind(jja[which(jja[,7]==4),c(1,4:6)], mam[which(mam[,7]==4), 4:5], djf[which(djf[,7]==4), 4])
colnames(jja2_zone4) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

# SPRING DATA (SON) starts in FIRST year bc we have winter(JJA) and fall(MAM) from that year
son2 = cbind(son[,c(1,4:6)], jja[, 4:5], mam[, 4])
colnames(son2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

son2_zone1 = cbind(son[which(son[,7]==1),c(1,4:6)], jja[which(jja[,7]==1), 4:5], mam[which(mam[,7]==1), 4])
colnames(son2_zone1) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

son2_zone2 = cbind(son[which(son[,7]==2),c(1,4:6)], jja[which(jja[,7]==2), 4:5], mam[which(mam[,7]==2), 4])
colnames(son2_zone2) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

son2_zone3 = cbind(son[which(son[,7]==3),c(1,4:6)], jja[which(jja[,7]==3), 4:5], mam[which(mam[,7]==3), 4])
colnames(son2_zone3) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")

son2_zone4 = cbind(son[which(son[,7]==4),c(1,4:6)], jja[which(jja[,7]==4), 4:5], mam[which(mam[,7]==4), 4])
colnames(son2_zone4) = c("cell","precip",  "evi", "production",
                         "precip_lag1",  "evi_lag1", "precip_lag2")


# use sample to run the OLS 
djf2_zone2_subset <- subset(djf2_zone2, djf2_zone2$production != 3)
djf2_zone2_sample <- ddply(djf2_zone2_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
djf2_zone3_subset <- subset(djf2_zone3, djf2_zone3$production != 3)
djf2_zone3_sample <- ddply(djf2_zone3_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
djf2_zone4_subset <- subset(djf2_zone4, djf2_zone4$production != 3)
djf2_zone4_sample <- ddply(djf2_zone4_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])

mam2_zone2_subset <- subset(mam2_zone2, mam2_zone2$production != 3)
mam2_zone2_sample <- ddply(mam2_zone2_subset, .(cell,production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
mam2_zone3_subset <- subset(mam2_zone3, mam2_zone3$production != 3)
mam2_zone3_sample <- ddply(mam2_zone3_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
mam2_zone4_subset <- subset(mam2_zone4, mam2_zone4$production != 3)
mam2_zone4_sample <- ddply(mam2_zone4_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])

jja2_zone2_subset <- subset(jja2_zone2, jja2_zone2$production != 3)
jja2_zone2_sample <- ddply(jja2_zone2_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
jja2_zone3_subset <- subset(jja2_zone3, jja2_zone3$production != 3)
jja2_zone3_sample <- ddply(jja2_zone3_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
jja2_zone4_subset <- subset(jja2_zone4, jja2_zone4$production != 3)
jja2_zone4_sample <- ddply(jja2_zone4_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])

son2_zone2_subset <- subset(son2_zone2, son2_zone2$production != 3)
son2_zone2_sample <- ddply(son2_zone2_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
son2_zone3_subset <- subset(son2_zone3, son2_zone3$production != 3)
son2_zone3_sample <- ddply(son2_zone3_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])
son2_zone4_subset <- subset(son2_zone4, son2_zone4$production != 3)
son2_zone4_sample <- ddply(son2_zone4_subset, .(production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),2:7])

# Save these datasets for use in RATS
#write.csv(djf2_zone1[,-1], sprintf("CHIRPS_djf2_zone1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(djf2_zone2_subset, djf2_zone2_subset$production == 1), sprintf("CHIRPS_djf2_zone2_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(djf2_zone2_subset, djf2_zone2_subset$production == 2), sprintf("CHIRPS_djf2_zone2_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(djf2_zone3_subset, djf2_zone3_subset$production == 1), sprintf("CHIRPS_djf2_zone3_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(djf2_zone3_subset, djf2_zone3_subset$production == 2), sprintf("CHIRPS_djf2_zone3_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(djf2_zone4_subset, djf2_zone4_subset$production == 1), sprintf("CHIRPS_djf2_zone4_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(djf2_zone4_subset, djf2_zone4_subset$production == 2), sprintf("CHIRPS_djf2_zone4_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(mam2_zone2_subset, mam2_zone2_subset$production == 1), sprintf("CHIRPS_mam2_zone2_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(mam2_zone2_subset, mam2_zone2_subset$production == 2), sprintf("CHIRPS_mam2_zone2_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(mam2_zone3_subset, mam2_zone3_subset$production == 1), sprintf("CHIRPS_mam2_zone3_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(mam2_zone3_subset, mam2_zone3_subset$production == 2), sprintf("CHIRPS_mam2_zone3_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(mam2_zone4_subset, mam2_zone4_subset$production == 1), sprintf("CHIRPS_mam2_zone4_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(mam2_zone4_subset, mam2_zone4_subset$production == 2), sprintf("CHIRPS_mam2_zone4_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(jja2_zone2_subset, jja2_zone2_subset$production == 1), sprintf("CHIRPS_jja2_zone2_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(jja2_zone2_subset, jja2_zone2_subset$production == 2), sprintf("CHIRPS_jja2_zone2_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(jja2_zone3_subset, jja2_zone3_subset$production == 1), sprintf("CHIRPS_jja2_zone3_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(jja2_zone3_subset, jja2_zone3_subset$production == 2), sprintf("CHIRPS_jja2_zone3_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(jja2_zone4_subset, jja2_zone4_subset$production == 1), sprintf("CHIRPS_jja2_zone4_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(jja2_zone4_subset, jja2_zone4_subset$production == 2), sprintf("CHIRPS_jja2_zone4_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(son2_zone2_subset, son2_zone2_subset$production == 1), sprintf("CHIRPS_son2_zone2_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(son2_zone2_subset, son2_zone2_subset$production == 2), sprintf("CHIRPS_son2_zone2_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(son2_zone3_subset, son2_zone3_subset$production == 1), sprintf("CHIRPS_son2_zone3_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(son2_zone3_subset, son2_zone3_subset$production == 2), sprintf("CHIRPS_son2_zone3_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

write.csv(subset(son2_zone4_subset, son2_zone4_subset$production == 1), sprintf("CHIRPS_son2_zone4_lps1_%s_%s.csv", args$time, args$type), quote =F, row.names = F)
write.csv(subset(son2_zone4_subset, son2_zone4_subset$production == 2), sprintf("CHIRPS_son2_zone4_lps2_%s_%s.csv", args$time, args$type), quote =F, row.names = F)

djf2_rshp <- aggregate(.~cell, djf2, FUN=c)
djf2_df <- do.call(cbind.data.frame, djf2_rshp)
mam2_rshp <- aggregate(.~cell, mam2, FUN=c)
mam2_df <- do.call(cbind.data.frame, mam2_rshp)

jja2_rshp <- aggregate(.~cell, jja2, FUN=c)
jja2_df <- do.call(cbind.data.frame, jja2_rshp)
son2_rshp <- aggregate(.~cell, son2, FUN=c)
son2_df <- do.call(cbind.data.frame, son2_rshp)


djf2_zone1_rshp <- aggregate(.~cell, djf2_zone1, FUN=c)
djf2_zone1_df <- do.call(cbind.data.frame, djf2_zone1_rshp)
djf2_zone2_rshp <- aggregate(.~cell, djf2_zone2, FUN=c)
djf2_zone2_df <- do.call(cbind.data.frame, djf2_zone2_rshp)
djf2_zone3_rshp <- aggregate(.~cell, djf2_zone3, FUN=c)
djf2_zone3_df <- do.call(cbind.data.frame, djf2_zone3_rshp)
djf2_zone4_rshp <- aggregate(.~cell, djf2_zone4, FUN=c)
djf2_zone4_df <- do.call(cbind.data.frame, djf2_zone4_rshp)

mam2_zone1_rshp <- aggregate(.~cell, mam2_zone1, FUN=c)
mam2_zone1_df <- do.call(cbind.data.frame, mam2_zone1_rshp)
mam2_zone2_rshp <- aggregate(.~cell, mam2_zone2, FUN=c)
mam2_zone2_df <- do.call(cbind.data.frame, mam2_zone2_rshp)
mam2_zone3_rshp <- aggregate(.~cell, mam2_zone3, FUN=c)
mam2_zone3_df <- do.call(cbind.data.frame, mam2_zone3_rshp)
mam2_zone4_rshp <- aggregate(.~cell, mam2_zone4, FUN=c)
mam2_zone4_df <- do.call(cbind.data.frame, mam2_zone4_rshp)

jja2_zone1_rshp <- aggregate(.~cell, jja2_zone1, FUN=c)
jja2_zone1_df <- do.call(cbind.data.frame, jja2_zone1_rshp)
jja2_zone2_rshp <- aggregate(.~cell, jja2_zone2, FUN=c)
jja2_zone2_df <- do.call(cbind.data.frame, jja2_zone2_rshp)
jja2_zone3_rshp <- aggregate(.~cell, jja2_zone3, FUN=c)
jja2_zone3_df <- do.call(cbind.data.frame, jja2_zone3_rshp)
jja2_zone4_rshp <- aggregate(.~cell, jja2_zone4, FUN=c)
jja2_zone4_df <- do.call(cbind.data.frame, jja2_zone4_rshp)

son2_zone1_rshp <- aggregate(.~cell, son2_zone1, FUN=c)
son2_zone1_df <- do.call(cbind.data.frame, son2_zone1_rshp)
son2_zone2_rshp <- aggregate(.~cell, son2_zone2, FUN=c)
son2_zone2_df <- do.call(cbind.data.frame, son2_zone2_rshp)
son2_zone3_rshp <- aggregate(.~cell, son2_zone3, FUN=c)
son2_zone3_df <- do.call(cbind.data.frame, son2_zone3_rshp)
son2_zone4_rshp <- aggregate(.~cell, son2_zone4, FUN=c)
son2_zone4_df <- do.call(cbind.data.frame, son2_zone4_rshp)

#seas2_zone_list_1 <- list(djf2_zone1_df, djf2_zone2_df, djf2_zone3_df, mam2_zone1_df, mam2_zone2_df, mam2_zone3_df)
#names(seas2_zone_list_1) <- c("djf2_zone1", "djf2_zone2","djf2_zone3","mam2_zone1","mam2_zone2","mam2_zone3")

seas2_zone_list_1 <- list(djf2_df, djf2_zone2_df, djf2_zone3_df, djf2_zone4_df,mam2_df, mam2_zone2_df, mam2_zone3_df, mam2_zone4_df)
names(seas2_zone_list_1) <- c("djf2","djf2_zone2","djf2_zone3","djf2_zone4","mam2","mam2_zone2","mam2_zone3","mam2_zone4")

# write out raster files 
for (i in 1:length(seas2_zone_list_1)){
  seas2_raster <- brick(nrows=1380, ncols=940, nl=72, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  
  seas2_raster[as.numeric(as.character(seas2_zone_list_1[[i]][,1]))] <- as.matrix(seas2_zone_list_1[[i]][,-1])
  writeRaster(seas2_raster, file=paste(names(seas2_zone_list_1)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
}


seas2_zone_list_2 <- list(jja2_df, jja2_zone2_df, jja2_zone3_df, jja2_zone4_df, son2_df, son2_zone2_df, son2_zone3_df,son2_zone4_df)
names(seas2_zone_list_2) <- c("jja2","jja2_zone2","jja2_zone3","jja2_zone4","son2", "son2_zone2","son2_zone3","son2_zone4")

# write out raster files 
for (i in 1:length(seas2_zone_list_2)){
  seas2_raster <- brick(nrows=1380, ncols=940, nl=78, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  
  seas2_raster[as.numeric(as.character(seas2_zone_list_2[[i]][,1]))] <- as.matrix(seas2_zone_list_2[[i]][,-1])
  writeRaster(seas2_raster, file=paste("CHIRPS",names(seas2_zone_list_2)[i], ".hdr", sep=""), format="ENVI", overwrite=TRUE)
}

#head(sort(tapply(mam[,4],mam[,1],  sum)), 30)

i <- 200
i <- 700
par(mar = c(5,5,2,5))
plot(as.numeric(total_filtered_sample[i,2:53]), type="l", col="black", 
     ylim=c(0,1), ylab = "EVI", xaxt="n", xlab="",main="Rangeland-based", lwd=2.25, cex.axis=1.5, cex.lab=1.45)
axis(1, at=c(1,5,9,13,17, 21, 25, 29, 33, 37, 41, 45, 49), labels=2003:2015,cex.axis=1.5, cex.lab=1.45)
#lines(as.numeric(total_filtered_sample[1,53:104]), type="l", col="red")
par(new=T)
plot(as.numeric(total_filtered_sample[i,54:105]), type="l", col="red", lwd=2.25, lty=2,ylab=NA, xlab=NA, axes=F, ylim=c(0,1000))
axis(side=4, cex.axis=1.5)
mtext(side =4, line=3, "Precipitation", cex=1.45)
legend("topleft", legend=c(expression("EVI", "Precip")), lty=c(1,2), col=c("black", "red"), lwd=3, cex=1.5, bty="n")