source("/projectnb/modislc/users/rkstan/GE712/GE712/scripts/apply_stack_parallel.R")

# load libraries 
library(RColorBrewer)
library(raster)
library(maptools)


# function to set all irrelevant classes to NA. Keep only the classes listed here
fun <- function(x) {x[!x==30 & !x==40 & !x==110 & !x==130 & !x==140 & !x==150 
                      & !x==151 & !x==152 & !x==153] <- NA; return(x)}

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# #read CCI data for South America and select only time period of interest (2003-2015)
# #cci <- stack("/projectnb/modislc/users/rkstan/GE712/data/CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif")
# cci_sa <- stack("/projectnb/modislc/users/rkstan/GE712/data/CCI/land_cover_change_sa.tif")
# #cci_time <- cci[[12:24]]
# cci_sa_time <- cci_sa[[12:24]] 
# 
# # Keep only the classes we need and save the raster
# cci_pasture_sub <-  calc(cci_sa_time, fun)
# setwd("/projectnb/modislc/users/rkstan/GE712/outputs")
# writeRaster(cci_pasture_sub, file= "cci_pasture_sub.hdr", format="ENVI", overwrite=TRUE)

# read in raster subset (in order to avoid running the first part again as it is slow!)
cci_pasture_sub <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/cci_pasture_sub.envi")

get_stat <- function(x, args.list){
  
  res <- rep(NA, args.list$nl)
  
 # if (anyNA(x))
#    return(res)
  
  n <- sum(is.na(x))
  x[n>4]<-NA
  x[is.na(x)] <- -9999
  unique_vals = length(unique(x))
  res <- c(uv = unique_vals)

return(res)

} 

lcc_mask = apply_stack_parallel(cci_pasture_sub, fun =get_stat, nl = 1)

# plot ------------------------------------------------------------------

cbPallete <- c(brewer.pal(6, "Paired"))
brks <- c(30, 40, 110, 130, 150,cellStats(cci_pasture_sub[[1]],max))
cci_pasture_breaks <- cut(cci_pasture_sub[[1]], breaks=brks)
plot(cci_pasture_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('30', '40', '110', 
                                            '130', '150', '153'), horiz=T)

cbPallete <- c(brewer.pal(3, "Set1"))
brks <- c(1,2,3)
lcc_mask_breaks <- cut(lcc_mask, breaks=brks)
plot(lcc_mask_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('1', '2', '3'), horiz=T)

# Extras ------------------------------------------------------------------
#b4 <- calc(cci_pasture_sub, fun=function(x,...){length(unique(x))})
#lcc_mask <- calc(cci_pasture_sub, fun=function(x,...){ if (is.na(x[1])){ NA } else {length(unique(x))}})
#lcc_mask <- calc(cci_pasture_sub, fun=function(x,...) {length(unique(na.pass(x)))})

#b1 <- calc(cci_pasture_sub, fun=function(x){sum(x==30) | sum(x==40) | sum(x==110) | sum(x==130)| sum(x==140)|
#                                              sum(x==150) | sum(x==151) | sum(x==152) | sum(x==153)})

# writeRaster(b4, file= "lcc_mask.hdr", format="ENVI", overwrite=TRUE)
# n <- stackApply(cci_pasture_sub, indices=1, fun=function(x,...){sum(is.na(x))})
# cci_pasture_sub[is.na(cci_pasture_sub)] <- -9999
# 
