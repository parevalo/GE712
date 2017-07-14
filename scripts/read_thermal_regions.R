library(raster)
library(maptools)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# read in thermal data and output a matrix 

# set functions ----------------------

# make a matrix of all the z score values from a raster stack
get_raster_vals <- function(data){
  
  z_values <- matrix(NA, nrow=259200,ncol=dim(data)[3])
  for (i in 1:dim(data)[3]) {
    sub_values1 <- getValues(data[[i]])
    
    if(i==1){
      z_values <-sub_values1
      
    }else{
      z_values <- cbind(z_values, sub_values1)
      
    }
  }
  return(z_values)
  
}

thermal_regions <- raster("/projectnb/modislc/users/rkstan/GE712/data/thermal_regions_IIASA_v3/thermal_regimes_pastures.tif")

# combine classes 
# class 1 - tropics warm, tropics cool
# class 2 - subtropics warm, subtropics cool, subtropics cold, subtropics very cold 
# class 3 - temperate cool, temperate cold, temperate very cold 
# NA - boreal 

fun1 <- function(x) {x[x==1 | x==2] <- 1; return(x)}
thermal_sub_1 <-  calc(thermal_regions, fun1)
fun2 <- function(x) {x[x==3| x==4 | x==5 | x==6] <- 2; return(x)}
thermal_sub_2 <-  calc(thermal_sub_1, fun2)
fun3 <- function(x) {x[x==7| x==8 | x==9] <- 3; return(x)}
thermal_sub_3 <-  calc(thermal_sub_2, fun3)
fun4 <- function(x) {x[x==10] <- NA; return(x)}
thermal_sub_4 <-  calc(thermal_sub_3, fun4)


# plot the thermal regions! 
plot(thermal_sub_4, col=c("red", "black", "green"))
plot(sa, add=T,lty=1,lwd=0.5)

thermal_regions_vals <- get_raster_vals(thermal_sub_4)
write.csv(thermal_regions_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv", quote=FALSE, row.names=FALSE)
