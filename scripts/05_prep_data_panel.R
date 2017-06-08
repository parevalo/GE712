# Preparing data for RATS 

# load libraries
library(tidyverse)

setwd("/home/paulo/GE712/outputs")

# Read monthly and seasonal anomalies
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv")
temp_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv")
EVI_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies.csv")

precip_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv")
temp_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies.csv")
EVI_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies.csv")

# Merge and remove rows with at least one NA
total_monthly <- cbind(EVI_monthly_anomalies, precip_monthly_anomalies, temp_monthly_anomalies)
total_seasonal <- cbind(EVI_seas_anomalies, precip_seas_anomalies, temp_seas_anomalies)

total_monthly_filtered = na.omit(total_monthly)
total_seas_filtered = na.omit(total_seasonal)

# Split variables back into separate df's
total_EVI_monthly <- as.data.frame(total_monthly_filtered[,1:156])
total_precip_monthly <- as.data.frame(total_monthly_filtered[,157:312])
total_temp_monthly <- as.data.frame(total_monthly_filtered[,313:468])

total_EVI_seas <- as.data.frame(total_seas_filtered[,1:52])
total_precip_seas <- as.data.frame(total_seas_filtered[,53:104])
total_temp_seas <- as.data.frame(total_seas_filtered[,105:156])


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
tidy_precip_monthly_anom = tidy_data(total_precip_monthly, "month")
tidy_temp_monthly_anom = tidy_data(total_temp_monthly, "month")
tidy_EVI_monthly_anom = tidy_data(total_EVI_monthly, "month")

tidy_precip_seas_anom = tidy_data(total_precip_seas, "season")
tidy_temp_seas_anom = tidy_data(total_temp_seas, "season")
tidy_EVI_seas_anom = tidy_data(total_EVI_seas, "season")


# Merge into months and seasons
full_dataset_month = cbind(tidy_precip_monthly_anom, tidy_temp_monthly_anom$value, tidy_EVI_monthly_anom$value)
colnames(full_dataset_month) = c("ID", "month", "precip_anom", "temp_anom", "evi_anom")

full_dataset_season = cbind(tidy_precip_seas_anom, tidy_temp_seas_anom$value, tidy_EVI_seas_anom$value)
colnames(full_dataset_season) = c("ID", "season", "precip_anom", "temp_anom", "evi_anom")


# Get season indices

djf_index = grep("DJF", full_dataset_season$season)
mam_index = grep("MAM", full_dataset_season$season)
jja_index = grep("JJA", full_dataset_season$season)
son_index = grep("SON", full_dataset_season$season)

# Subset the seasons!
djf = full_dataset_season[djf_index,]
mam = full_dataset_season[mam_index,]
jja = full_dataset_season[jja_index,]
son = full_dataset_season[son_index,]

# Save full dataset and seasonal with variables only so that RATS can read it
write.csv(full_dataset_season[,3:5], "panel_seasonal_dataset.csv", quote =F, row.names = F)
write.csv(full_dataset_month[,3:5], "panel_monthly_dataset.csv", quote =F, row.names = F)
write.csv(djf[,3:5], "djf.csv", quote =F, row.names = F)
write.csv(mam[,3:5], "mam.csv", quote =F, row.names = F)
write.csv(jja[,3:5], "jja.csv", quote =F, row.names = F)
write.csv(son[,3:5], "son.csv", quote =F, row.names = F)


# Rename var columns for each season so that we can use values from previous seasons
# as a different "variable" (instead of having to deal with RATS lagging). Format ready for RATS

# SUMMER DATA (DJF) starts in SECOND year bc we need spring(SON) and winter (JJA) from previous year
# so we need to remove the first DJF year FROM ALL INDIVIDUALS and the last from SON and JJA

djf_sub = filter(djf, season != "DJF")
son_sub = filter(son, season != "SON.12")
jja_sub = filter(jja, season != "JJA.12")

djf2 = cbind(djf_sub[,3:5], son_sub[,3:4], jja_sub[,3:4])
colnames(djf2) = c("precip_anom_djf", "temp_anom_djf", "evi_anom_djf", 
                   "precip_anom_son", "temp_anom_son", "precip_anom_jja", "temp_anom_jja")

# FALL DATA (MAM) starts in SECOND year bc we need summer(DJF) and spring (SON) from previous year
# so we need to remove the first MAM year FROM ALL INDIVIDUALS and the last from DJF and SON
mam_sub = filter(mam, season != "MAM")
djf_sub = filter(djf, season != "DJF.12")
son_sub = filter(son, season != "SON.12")

mam2 = cbind(mam_sub[,3:5], djf_sub[,3:4], son_sub[,3:4])
colnames(mam2) = c("precip_anom_mam", "temp_anom_mam", "evi_anom_mam", 
                   "precip_anom_djf", "temp_anom_djf", "precip_anom_son", "temp_anom_son")

# WINTER DATA (JJA) starts in FIRST year bc we have fall(MAM) and summer(DJF) from that year
jja2 = cbind(jja[,3:5], mam[, 3:4], djf[, 3:4])
colnames(jja2) = c("precip_anom_jja", "temp_anom_jja", "evi_anom_jja", 
                   "precip_anom_mam", "temp_anom_mam", "precip_anom_djf", "temp_anom_djf")

# SPRING DATA (SON) starts in FIRST year bc we have winter(JJA) and fall(MAM) from that year
son2 = cbind(son[,3:5], jja[, 3:4], mam[, 3:4])
colnames(son2) = c("precip_anom_son", "temp_anom_son", "evi_anom_son", 
                   "precip_anom_jja", "temp_anom_jja", "precip_anom_mam", "temp_anom_mam")


# Save these datasets for use in RATS

write.csv(djf2, "djf2.csv", quote =F, row.names = F)
write.csv(mam2, "mam2.csv", quote =F, row.names = F)
write.csv(jja2, "jja2.csv", quote =F, row.names = F)
write.csv(son2, "son2.csv", quote =F, row.names = F)