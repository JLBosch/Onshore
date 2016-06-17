#-----------------------------------------------------------------------------------#
#	Program: XYZtoGTiff.R					                                            		  	#
#	> Turns CSV data with equally spaced grid to GTiff raster with same CRS           #	
#                     														                              	  #		
#-----------------------------------------------------------------------------------#



## load packages

library(sp)
library(raster)
library(rgdal)





## INPUT DIRECTORIES ##

dir.europe <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Europe"

dir.oceana <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Oceania"
  
dir.southA <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\South America"







## SET WORKING DIRECTORY ##

setwd( dir.southA )








## FUNCTION to get CF files in some order ##

get.CF.file <- function(directory){
  
  csv.country <- list.files( path=getwd(), pattern= ".CSV$" , full.names= T )
  
  return(csv.country)
  
}








## FUNCTION to make raster files ##

for( i in 1:length( get.CF.file(getwd() ) ) ){ 


              ## read CSV ##
                
              XYZdata <- read.csv( file= get.CF.file(getwd())[i],
                              header= TRUE,
                              sep= ",",
                              dec= "."
                              )[,2:35]   ## skip NAME column ##
              
              ## create Raster files ## 
              
              ras <- rasterFromXYZ( XYZdata, 
                                    crs= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ), 
                                    digits=5)
              
              writeRaster( ras, 
                           filename= paste0( sub( pattern= "CSV+$", replace="", get.CF.file(getwd())[i] ) ), 
                           format= "GTiff",
                           overwrite= TRUE)
              
}



## TEST OUTPUT ##

r <- raster( paste0( sub( pattern= "CSV+$", replace="", get.CF.file(getwd())[i] ), "tif" ), 2 )
plot(r)



## FOR MERGING CSV FILES ##

merge(x,y,...)


## END ##

