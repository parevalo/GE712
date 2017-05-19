library(raster)
library(rgdal)
library(RColorBrewer)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")

# Read in stacks of evi, precip and temp, 13 layers each
djf_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_raster.envi")
mam_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_raster.envi")
jja_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_raster.envi")
son_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_raster.envi")

## RUN PIXELWISE OLS AND MAP COEFFICIENTS

# Prepare stacks by masking them so that they have the same data/nodata areas
prep_stacks = function(rast){
  evi = subset(rast, 1:13)
  precip = subset(rast, 14:26)
  temp = subset(rast, 27:39)
  
  # Mask them to have the same areas of data/nodata
  precip_masked = mask(precip, temp)
  evi_masked = mask(evi, precip_masked)
  out_rast = stack(evi_masked, precip_masked, temp)
  return(out_rast)

}

djf = prep_stacks(djf_raster)
mam = prep_stacks(mam_raster)
jja = prep_stacks(jja_raster)
son = prep_stacks(son_raster)


# Define functions to obtain each of the coefficients from the regression

# 1 to 6: Get intercepts and slopes from evi ~ precip + temp + precip^2 + temp^2. Get r squares too.
get_intercept=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26])$coefficients[1] }}

get_beta_precip=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[2] }}
get_beta_temp=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[3] }}
get_beta_precip2=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[4] }}
get_beta_temp2=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[5] }}

get_rsquared=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$r.squared }}

# 7 to 10:Get significance level for each of the coefficients
get_signif_precip=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[2,4]}}
get_signif_temp=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[3,4]}}
get_signif_precip2=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[4,4]}}
get_signif_temp2=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[4,4]}}

# 11 to 14: Implements functions to calculate the optimum
get_beta_precip_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2))$coefficients[2] }}
get_beta_precip2_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2))$coefficients[3] }}
get_beta_temp_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2))$coefficients[2] }}
get_beta_temp2_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2))$coefficients[3] }}

# 15 to 18: Get significance of those optimum coefficients
get_signif_precip_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2));summary(m)$coefficients[2,4] }}
get_signif_precip2_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2));summary(m)$coefficients[3,4] }}
get_signif_temp_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2));summary(m)$coefficients[2,4] }}
get_signif_temp2_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2));summary(m)$coefficients[3,4] }}


# Put all functions on a vector to make it easier to calculate them
fnc_list = c(get_intercept, get_beta_precip, get_beta_temp, get_beta_precip2, get_beta_temp2, get_rsquared, 
             get_signif_precip, get_signif_temp, get_signif_precip2, get_signif_temp2, 
             get_beta_precip_opt,get_beta_precip2_opt, get_beta_temp_opt, get_beta_temp2_opt, 
             get_signif_precip_opt, get_signif_precip2_opt, get_signif_temp_opt, get_signif_temp2_opt)


# Iterate over functions and calculate each of the coefficients per season 
calc_coefs = function(x){
  coef_list = list()
  for(i in 1:length(fnc_list)){
    coef_list[[i]] = calc(x, fnc_list[[i]])
  }  
  return(coef_list)
}

# Get ALL coefficients per season
djf_coefs = calc_coefs(djf)
mam_coefs = calc_coefs(mam)
jja_coefs = calc_coefs(jja)
son_coefs = calc_coefs(son)

# Fnc to calculate optimum values
calculate_optimum = function(season_coefs){
  # Create masks with threshold for significance
  filter_signif = function(x){ x[x > 0.1] = NA; return(x)}
  precip_mask = calc(season_coefs[[15]], filter_signif)
  precip2_mask = calc(season_coefs[[16]], filter_signif)
  temp_mask = calc(season_coefs[[17]], filter_signif)
  temp2_mask = calc(season_coefs[[18]], filter_signif)
  
  # Filter actualy coefficients using the masks we created
  precip_masked = mask(season_coefs[[11]], precip_mask)
  precip2_masked = mask(season_coefs[[12]], precip2_mask)
  temp_masked = mask(season_coefs[[13]], temp_mask)
  temp2_masked = mask(season_coefs[[14]], temp2_mask)
  
  # Get optimum values
  optimum = list()
  optimum[[1]] = 2*temp2_masked  # 2nd derivative temp
  optimum[[2]] = (-1 * temp_masked) / optimum[[1]] # temp optimum, -1 required to avoid error
  optimum[[3]] = 2*precip2_masked # 2nd derivative precip
  optimum[[4]] = (-1 * precip_masked) / optimum[[3]] # precip optimum, -1 required to avoid error
  
  return(optimum)
}


# Calculate optimum values
djf_optimum = calculate_optimum(djf_coefs)
mam_optimum = calculate_optimum(mam_coefs)
jja_optimum = calculate_optimum(jja_coefs)
son_optimum = calculate_optimum(son_coefs)


## CALCULATE AND MAP ECOLOGICAL OPTIMUM
#equations
# evi = a + b1*temp + b2*(temp^2)
### take first derivative 
# evi = b1*(temp^0) + b2*2*(temp^1)
# evi = b1 + 2*b2*temp
### find critical values 
# 0 = b1 + 2*b2*temp
# temp = -b1/(2*b2)
### take second derivative 


