# Preparing data for RATS 

# load libraries
library(tidyverse)
library(xlsx)

# Input folders
# read in and clean monthly data 
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv")
temp_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv")
EVI_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies.csv")
total_original <- cbind(EVI_monthly_anomalies, precip_monthly_anomalies, temp_monthly_anomalies)
f = na.omit(total_original)

total_EVI <- as.data.frame(f[,2:157])
total_precip <- as.data.frame(f[,159:314])
total_temp <- as.data.frame(f[,316:471])

# read in and clean seasonall data 
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv")
temp_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies.csv")
EVI_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies.csv")
total_original <- cbind(EVI_seas_anomalies, precip_seas_anomalies, temp_seas_anomalies)
f = na.omit(total_original)

total_EVI <- as.data.frame(f[,2:53])
total_precip <- as.data.frame(f[,55:106])
total_temp <- as.data.frame(f[,108:159])

# Function to tidy each of the dataframes read from the csv files. 
tidy_data = function(df){
  # Rename columns to make data wrangling easier, and add row ID's and r
  # Temporary colnames, data goes from 2003 to 2015
  colnames(df) = paste0("period", seq(1, ncol(df)))
  df$ID = seq(1, nrow(df))
  
  # Gather into rows and assign proper column names
  temp_df = gather(df, key="ID", value="value")
  head(temp_df)
  colnames(temp_df) = c("ID", "period", "value")
  temp_df_sorted =  arrange(temp_df, ID)
}

# Tidy each dataset
tidy_precip_anom = tidy_data(total_precip)
tidy_temp_anom = tidy_data(total_temp)
tidy_EVI_anom = tidy_data(total_EVI)

# Merge into a single dataset and rename columns
full_dataset = cbind(tidy_precip_anom, tidy_temp_anom$value, tidy_EVI_anom$value)
colnames(full_dataset) = c("ID", "period", "precip_anom", "temp_anom", "evi_anom")
full_dataset_sub = full_dataset[,3:5]


write.csv(full_dataset_sub, file="/projectnb/modislc/users/rkstan/GE712/outputs/full_dataset_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)
write.csv(full_dataset_sub, file="/projectnb/modislc/users/rkstan/GE712/outputs/full_dataset_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

write.xlsx(full_dataset, file="/projectnb/modislc/users/rkstan/GE712/outputs/full_dataset_monthly_anomalies.xlsx")
