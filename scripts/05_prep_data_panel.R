# Preparing data for RATS 

# load libraries
library(tidyverse)

setwd("/home/paulo/GE712/outputs")

# Read monthly and seasonal anomalies
precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
temp_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/temp_monthly_anomalies.csv")
EVI_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/EVI_monthly_anomalies.csv")

precip_seas_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_seas_anomalies.csv")
temp_seas_anomalies <- read.csv(file="/home/paulo/GE712/outputs/temp_seas_anomalies.csv")
EVI_seas_anomalies <- read.csv(file="/home/paulo/GE712/outputs/EVI_seas_anomalies.csv")


# Function to tidy each of the dataframes read from the csv files. 
tidy_data = function(df, period_name){
  # Create ID column
  df$ID = seq(1, nrow(df))
  
  # Gather into rows and assign proper column names
  temp_df = gather(df, key="ID", value="value")
  head(temp_df)
  colnames(temp_df) = c("ID", period_name, "value")
  temp_df_sorted =  arrange(temp_df, ID)
}

# Tidy months and seasons
tidy_precip_monthly_anom = tidy_data(precip_monthly_anomalies, "month")
tidy_temp_monthly_anom = tidy_data(temp_monthly_anomalies, "month")
tidy_EVI_monthly_anom = tidy_data(EVI_monthly_anomalies, "month")

tidy_precip_seas_anom = tidy_data(precip_seas_anomalies, "season")
tidy_temp_seas_anom = tidy_data(temp_seas_anomalies, "season")
tidy_EVI_seas_anom = tidy_data(EVI_seas_anomalies, "season")


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

# Save full dataset and seasonal
write.csv(full_dataset_season, "panel_seasonal_dataset.csv")
write.csv(full_dataset_month, "panel_monthly_dataset.csv")
write.csv(djf, "djf.csv")
write.csv(mam, "mam.csv")
write.csv(jja, "jja.csv")
write.csv(son, "son.csv")

