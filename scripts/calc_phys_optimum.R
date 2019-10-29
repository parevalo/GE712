#taking the derivative to find the physiological optimum 

library(raster)

#read in rasters 
djf_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_raster.envi")
mam_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_raster.envi")
jja_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_raster.envi")
son_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_raster.envi")

#equations
# evi = a + b1*temp + b2*(temp^2)
### take first derivative 
# evi = b1*(temp^0) + b2*2*(temp^1)
# evi = b1 + 2*b2*temp
### find critical values 
# 0 = b1 + 2*b2*temp
# temp = -b1/(2*b2)
### take second derivative 


# for temperature
lm_temp <- lm(djf$evi ~ djf$temp+I(djf$temp^2))
b2_temp <- summary(lm_temp)$coefficients[2]
b3_temp <- summary(lm_temp)$coefficients[3]

# for precipitation
lm_precip <- lm(djf$evi ~ djf$precip+I(djf$precip^2))
b2_precip <- summary(lm_precip)$coefficients[2]
b3_precip <- summary(lm_precip)$coefficients[3]

#taking the derivative to find the critical values 
temp <- -b2_temp/(2*b3_temp)
precip <- -b2_precip/(2*b3_precip)
