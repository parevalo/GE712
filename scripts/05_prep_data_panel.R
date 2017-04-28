# preparing data for RATS 

# load libraries
library(tidyr)
library(dplyr)

precip_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv")
temp_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv")
EVI_monthly_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies.csv")