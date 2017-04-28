# Preparing data for RATS 

# load libraries
library(tidyverse)

# Input folders
precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
temp_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/temp_monthly_anomalies.csv")
EVI_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/EVI_monthly_anomalies.csv")


# Function to tidy each of the dataframes read from the csv files. 
tidy_data = function(df){
  # Rename columns to make data wrangling easier, and add row ID's and r
  # Temporary colnames, data goes from 2003 to 2015
  colnames(df) = paste0("month", seq(1, ncol(df)))
  df$ID = seq(1, nrow(df))
  
  # Gather into rows and assign proper column names
  temp_df = gather(df, key="ID", value="value")
  head(temp_df)
  colnames(temp_df) = c("ID", "month", "value")
  temp_df_sorted =  arrange(temp_df, ID)
}

# Tidy each dataset
tidy_precip_anom = tidy_data(precip_monthly_anomalies)
tidy_temp_anom = tidy_data(temp_monthly_anomalies)
tidy_EVI_anom = tidy_data(EVI_monthly_anomalies)

# Merge into a single dataset and rename columns
full_dataset = cbind(tidy_precip_anom, tidy_temp_anom$value, tidy_EVI_anom$value)
colnames(full_dataset) = c("ID", "month", "precip_anom", "temp_anom", "evi_anom")


