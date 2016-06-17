

#----------------------------------------------#
#					                           #
#  TechnicalPotential.R                        #
#                                              #
#  > Calculates Technical potential            #
#                                              #
#   1. input datasets                          #
#   2. output directories                      #
#   3. get countries NaturalEarth              #
#   4. geographical restrictions               #
#       4a. snip datasets to country borders   # 
#           & exclude conservation shp.s       #
#       4b. resample to same resolution        #
#   5. suitability factor function, Fsub_i     #
#       5a. topology exclusions                #
#       5b. land use suitability               #
#   6. Interpolate CF to TOPO resolution       #
#       6a. Print CF maps with exclusions      #
#   7. Technical potential function, Esub_i    #
#       7a. turbine packing density            #
#       7b. availability/efficiency            #
#       7c. Generation potential               #
#                                              #
#----------------------------------------------#





#-----------------------------------#
#					                          #
#  Load libraries/packages          #
#                                   #
#-----------------------------------# 


if (!require(C("RColorBrewer", "ggplot2") )) {
  install.packages("RColorBrewer")
  install.packages("ggplot2")
  library(RColorBrewer)
  library(ggplot2)
}


library(ncdf4)
library(sp)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)







## BEGIN SCRIPT ##


#-----------------------------------#
#					                          #
#  1. Input datasets                #
#                                   #
#-----------------------------------# 



## GTiff DATA ##

## topology: GTOPO30 
## land cover: GlobCover 2009


tif.topo <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R DATA\\GTOPO30\\GTOPO30_Global.tif"
tif.land <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R DATA\\Globcover2009_V2.3_Global_\\GLOBCOVER_L4_200901_200912_V2.3.tif"






## SHAPE DATA ##

## protected areas: The World Database on Protected Areas (WDPA)


shp.countries <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R DATA\\NaturalEarthCountries" ##IMPORTANT: without final "\\"
shp.cons <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R DATA\\WDPA_May2016-shapefile"









#-----------------------------------#
#					                #
#  2. Output directories            #
#                                   #
#-----------------------------------#





## general directory for GTiffs ##

dir.out.rasters <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R GTiffs\\Rasters by Country"
dir.out.GTiff <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R GTiffs\\"




## for specific final results ##

dir.out.TP <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R GTiffs\\Rasters by Country\\TechPotentialMasks"
dir.out.stats <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R OUTPUT\\TechPotential"
dir.out.CF <- "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R GTiffs\\CFs"





#-----------------------------------#
#					                #
#  3. Get countries (ADM0_A3 names) #
#                                   #
#-----------------------------------#







## function to get Names of countries from shape files without extension ".shp" ##

get.names <- function(directory){
    
    shp.country <- list.files( path=directory, pattern= "\\.shp$" , full.names= F )
    
    country.names <- vector()
    for( i in 1:length(shp.country) ) { 
  
              country.names[i] <- paste( sub( "\\.[[:alnum:]]+$","", as.character( shp.country[i] ) ) )

    }
    
    return(country.names)
    
}






## get the number of countries from length of vector ##

country.names<- get.names(shp.countries)
no.countries <- length( country.names )
cat( no.countries )








#-----------------------------------#
#					                #
#  4. Geographical restrictions     #	
#					                #
#-----------------------------------#





## make raster objects from GTiff files ##

ras.topo <- raster( tif.topo )
ras.land <- raster( tif.land )





## make shape objects with country and conservation & area datasets ##


spatial.countries <- readOGR( dsn = shp.countries, layer =  )
spatial.cons <- readOGR( dsn = shp.cons, layer = "WDPA_May2016-shapefile-polygons" )


plot(spatial.cons)





## set Coordinate Reference System ##

crs( ras.topo ) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs( ras.land ) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

crs( spatial.countries ) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs( spatial.cons ) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"








## 4a. Exclude world conservation areas & datasets to country borders ##





cons.mask <- mask( ras.topo, spatial.cons, 
                   filename= paste0( dir.out.rasters, "TopoLessConsAreas"  ),
                   snap='out', overwrite= TRUE, inverse= TRUE )


plot(cons.mask)


## set-up cluster for parrallel processing ##

cl <- makeCluster( detectCores( all.tests=FALSE, logical=FALSE ) )
registerDoParallel(cl)




## first cut extent of global maps sequentially...

ptime <- system.time(
  {
    
    
    foreach( i= 1:no.countries, 
             .packages= c( "rgdal", "raster") ) %do% {
               
               ## collect region files and project in correct CRS	
               
               region_in <- readOGR( dsn = shp.countries, layer = country.names[i] )
               crs( region_in ) <- CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )
               

               
               ext <- extent( region_in )
               
               
               ## crop topology and land cover (gloabal) rasters to region extents & mask to dispose of outside-border values
               
               c.topo <- crop( cons.mask, ext, filename= "", snap='out' )
               m.topo <- mask( c.topo, region_in, 
                               filename= paste0( dir.out.rasters, "TopoMasks\\", "MaskTopo_", as.character( just.names[i] ), ".tif" ), 
                               overwrite= TRUE, inverse= FALSE )	
               
               ## and for land...
               
               c.land <- crop( ras.land, ext, filename= "", snap='out' )
               m.land <- mask( c.land, region_in, 
                               filename= paste0( dir.out.rasters, "LandMasks_Original\\", "MaskLand_", as.character( just.names[i] ), ".tif" ), 
                               overwrite= TRUE, inverse= FALSE )	
               
             }
  })        








## 4b. resample to same (topo) resolution ##




## list land and topology mask files
  
m.topo <- list.files( path= paste0( dir.out.rasters, "\\TopoMasks\\" ), pattern="MaskTopo_", full.names= F )   
m.land <- list.files( path= paste0( dir.out.rasters, "\\LandMasks_Original\\" ), pattern="MaskLand_", full.names= TRUE )   





## set-up cluster ##

cl <- makeCluster( detectCores( all.tests=FALSE, logical=FALSE ) )
registerDoParallel(cl)

ptime <- system.time(
  {
    
    foreach( i= 1:no.countries, 
             .packages= c( "rgdal", "raster") ) %dopar% {
               
               ## project topology extent with blank values 
               
               CRS_topo <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
               topo.template <- projectExtent( m.topo[i], CRS_topo ) 
               
               ## project rasters to topology resolution. land >> topo
               
               f <- paste0( paste0( dir.out.rasters, "LandMasks_Res\\" ), "MaskLand_Res_", as.character( country.names[i] ), ".tif")
               
               raster.land <- raster( m.land[i] )
               resample( raster.land, topo.template, method= "ngb",
                         filename = f,
                         overwrite= T ) 
               
             }
  })
stopCluster(cl=NULL)









#-----------------------------------#
#					                #
#  5. suitability factor function   #
#					                #
#-----------------------------------#




## FOR TOPOLOGY ##



## FUNCTION: for slopes > 20% raster value = NA ##

func.slope <- function(x) { 
    
  x[ x > 9 ] <- NA   ## 9 degrees = 20%
  
  return(x) 
  
} 

## FUNCTION: altitudes over 200m meter = NA ##

func.alt <- function(x) { 
  
  x[ x > 2000 ] <- NA   ## 2000m elevation

  return(x)
  
  
  
}




## loop through region rasters ##

cl <- makeCluster( detectCores(all.tests=FALSE, logical=FALSE) )
registerDoParallel(cl)

ptime <- system.time(
  {
    
    foreach( i= 1:no.countries, 
             .packages= c( "rgdal", "raster" ) ) %dopar% {
               
               ## create raster of topology for each ETP region: dir.out <- "L:\\Numbers and Analysis\\JBOSCH\\GIS\\REGION RASTERS\\"
               
               r.topo <- raster( paste0( dir.out.rasters, "\\TopoMasks\\", "MaskTopo_", as.character( country.names[i] ), ".tif" ))
               
               ## Exclude altitudes over 2000m ##
               
               alt.excl <- calc( r.topo, func.alt,
                                 filename= "",
                                 format= "GTiff", overwrite= TRUE )
               
               ## claculate slope in topology of each region ##
                
               slope <- terrain( alt.excl, opt= 'slope', unit= 'degrees', neighbors= 8, 
                                 filename= "" )
               
               ## mask -> new raster with cells > 20% slope assigned NA
               
               slope.excl <- calc( slope, func.slope, 
                                   filename= paste0( dir.out.rasters, "\\ExcludeMaps\\", "TopoMask_", as.character( country.names[i] ), ".tif" ),
                                   format= "GTiff", overwrite=TRUE )
               
               
             }
    
  })
stopCluster(cl) ##END SYSTEM TIME ## stop cluster








## FOR LAND CLASSES ##


## Suitability factors: from Globcover classes -> 0 SF as NA ##





func.land <- function(x){
  
  ifelse(x == 11, NA,
  ifelse(x == 14, 0.7,
  ifelse(x == 20, 0.8,
  ifelse(x == 30, 0.21,
  ifelse(x == 40, 0.1,
  ifelse(x == 50, 0.1,
  ifelse(x == 60, 0.5,
  ifelse(x == 70, 0.1,
  ifelse(x == 90, 0.1,
  ifelse(x == 100, 0.1,
  ifelse(x == 110, 0.51,
  ifelse(x == 120, 0.65,
  ifelse(x == 130, 0.5,
  ifelse(x == 140, 0.5,
  ifelse(x == 150, 0.8,
  ifelse(x == 160, NA,
  ifelse(x == 170, NA,
  ifelse(x == 180, NA,
  ifelse(x == 190, NA,
  ifelse(x == 200, 0.9,
  ifelse(x == 210, NA,
  ifelse(x == 220, NA,
  ifelse(x == 230, NA, x )	
                                                                                                    
  ))))))))))))))))))))))
  
}







## begin cluster for parallel processing ##

cl <- makeCluster( detectCores(all.tests=FALSE, logical=FALSE) )
registerDoParallel(cl)

ptime <- system.time(
  {
    
    foreach( i= 1:no.countries, 
             .packages= c( "rgdal", "raster") ) %dopar% {
               
               ## read mask file of land cover of each region: e.g. dir.out.raster
               
               r.land <- raster( paste0( dir.out.rasters, "\\LandMasks_Res\\","MaskLand_Res_", as.character( country.names[i] ), ".tif" ) )
               
               ## new raster with above land cover regions removed
               
               land.SF <- calc( r.land, func.land, 
                                  filename= paste0( dir.out.rasters, "\\ExcludeMaps\\", "LandMask_", as.character( country.names[i] ) ), 
                                  format= "GTiff", overwrite=TRUE )
               
             }
    
  })
stopCluster(cl) ##END SYSTEM TIME ## stop cluster









#-----------------------------------#
#					                #
#   Final country masks             #
#						            #
#-----------------------------------#





cl <- makeCluster( detectCores(all.tests= FALSE, logical= FALSE) )
registerDoParallel(cl)

ptime <- system.time(
  {
    
    foreach( i= 1:no.countries, 
             .packages= c( "rgdal", "raster") ) %dopar% {
               
               ## load (masked) slope and land rasters, and adjusted wind rasters
               
               slope.excl <- raster( paste0( dir.out.rasters, "\\ExcludeMaps\\","TopoMask_", as.character( country.names[i] ), ".tif" ) )
               land.SF <- raster( paste0( dir.out.rasters, "\\ExcludeMaps\\", "LandMask_", as.character( country.names[i] ), ".tif" ) )
               
               ## mask wind speed layer with 'sloped' and 'landed' masks
               
               FinalMask <- mask( land.SF, slope.excl, 
                            filename= paste0( dir.out.rasters, "\\FinalMasks\\", "Mask_", as.character( country.names[i] ), ".tif" ),
                            inverse= FALSE, maskvalue= NA, overwrite=TRUE )		
               
             }
    
  })
stopCluster(cl) ##END SYSTEM TIME ## stop cluster








#-----------------------------------#
#					                #
# 6. Interpolate CF to TOPO         #
#    resolution                     #
#						            #
#-----------------------------------#




dir.europe <-  "\\\\store.ic.ac.uk\\IC\\foe\\ese\\sgi\\Jonny\\R DATA\\CFmaps\\Europe" #"C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Europe"

dir.oceana <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Oceania"

dir.southA <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\South America"

dir.africa <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Africa"

dir.asia <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Asia"

dir.russia <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\Russia"

dir.southN <- "C:\\Users\\JB3814\\Box Sync\\IAIN&JONNY\\CAPACITY FACTORS\\North America"


## SET WORKING DIRECTORY ##

setwd( dir.europe )
getwd()








## time slice naming convention: [Season]_[slices per day]

Seasons <- c("WIN",
             "SPR",
             "SUM",
             "AUT")

SlicesPerDay <- c("H00", "H03", "H06", "H09", "H12", "H15", "H18", "H21")

no.slices <- length(Seasons) * length(SlicesPerDay)




## FUNCTION to get CF .tif files in some order ##

get.CF.tif <- function(directory){
  
  tif.country <- list.files( path= dir.europe, pattern= ".tif" , full.names= T )
  
  return(tif.country)
  
}

## FUNCTION to get country name from file names in a particular directory ##

get.country <- function(directory){
  
    files <- list.files( path=dir.europe, pattern= ".tif" , full.names= F )
    out_1 <- paste0( sub( pattern= "CAPFAC.", 
                          replace="", 
                          files ) )
    
    out_2 <- paste0( sub( pattern = ".tif", 
                          replace= "",
                          out_1 ) )
    return(out_2)
}







## OUTPUT FILE NAMES ##


slices <- matrix( nrow= length(SlicesPerDay), ncol= length(Seasons) )

for(i in 1:length(SlicesPerDay) ){
  
  for(j in 1:length(Seasons)){
  
  slices[i,j] <- paste0( 
                  as.character( Seasons[j] ), ".",
                  as.character( SlicesPerDay[i] ), ".tif") 
  
  }
  
}

## matrix of file names ##

f <- matrix( nrow= length(get.country( dir.europe )), ncol= length(slices))

for(i in 1:length(get.country( dir.europe ))){             
      for(j in 1:no.slices)   { 
            
f[i,j] <- paste0(dir.out.CF, 
                "\\CFINTERP.", 
                as.character( get.country( dir.europe )[i] ), ".", 
                as.character( slices[j] ) )
      }               
}            
   






## CRS ##

CRS_topo <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

## FUNCTION: increment n by 1 to read next file names

'%+=%' = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
n <- 0

## set-up cluster


cl <- makeCluster( detectCores( all.tests=FALSE, logical=FALSE ) )
registerDoParallel(cl)

ptime <- system.time(
  {
    
    # foreach( i= 8:length(get.country(dir.europe)),
    #          .combine='cbind', .options.nws=opts,
    #          .packages= c( "rgdal", "raster") ) %:% {
                   
       for( i in 1:length(get.country(dir.europe)) ) {
         
                   foreach( j= 1:length(slices),
                            .combine='c',
                            .packages= c( "rgdal", "raster") ) %dopar% {
                              
                              
                                        ## project topology extent with blank values 
                               
                               
                                        topo.template <- projectExtent( dir(path= paste0( dir.out.rasters, "\\TopoMasks" ), 
                                                               pattern= get.country(dir.europe)[i],full.names = TRUE), CRS_topo ) 
                                        ## create raster for each CAPFAC
                                          
                                        ras.cf <- raster( get.CF.tif( dir.europe )[i], as.numeric(j) )
                                                      
                                                      
                                        ## project rasters to topology resolution. CF >> topo ##            
                                      

                                        res <- resample( ras.cf, topo.template,
                                                        method= "bilinear",
                                                        filename = f[i,j],
                                                        overwrite= TRUE )
                                        
                      }
                                        
                                        
                              
                          
}
                          
                
})
stopCluster(cl=NULL)








## 6a. Print CF maps with exclusions ##



cl <- makeCluster( detectCores( all.tests=FALSE, logical=FALSE ) )
registerDoParallel(cl)


for(i in 1:length( get.country( dir.europe ))){
  
    ## prepare Final mask for each country ##
  
    ras.mask <- raster( dir( path= paste0( dir.out.rasters, "\\FinalMasks\\"),
                             pattern= paste0( '\\Mask_', get.country( dir.europe )[i] ), 
                             full.names= TRUE ) )
    
    foreach( j= 1:length(slices),
             .combine='c',
             .packages= c( "rgdal", "raster") ) %dopar% {
               
             ras.CF <- raster( dir( path= dir.out.CF,
                                    pattern= paste0( get.country( dir.europe )[i],".", slices[j] ), 
                                    full.names = TRUE ) )  
               
            
              
             ras.CF.RESULT <- overlay( ras.CF, ras.mask, fun= function(x,y){ x * y },
                                       filename= paste0( dir.out.GTiff, "\\CF_Overlay_Final\\", 
                                                         "CFout.", get.country( dir.europe )[i],".", slices[j] ),
                                       overwrite= TRUE )
                                  

             }
}







#-----------------------------------#
#					                #
# 7. Technical potential functionS  #
#						            #
#-----------------------------------#







## 7a. turbine packing density ##


P.nominal <- 3.975 ## [MW]





## Turbine model : Composite model

TurbineDia <- 110.6 ## [m]

TurbineDensity <-  1000^2 / (5 * TurbineDia * 10 * TurbineDia )  ## [turbines per km^2]







## 7b. availability  ##


Avail <- 0.97



## Nominal Power Statistics ##

PowerDensity <- Avail * P.nominal * TurbineDensity    ## [MW/km^2]

Ann.Gen <- Avail * P.nominal * 3600 * 8760     ## [MWh/ann]

NomGenDens <- Ann.Gen * TurbineDensity   ## nominal energy density [MWh / km^2 / ann]









## 7c. Generation potential  ##






## Calculate CAPACITY in each CF band per region and time slice

n.CFs <- 0:9    ## number of CF bands = 10 ##






## FUNCTION to create 10 CF bands with 5% steps 

CF.func <- function(x)
{

    
  x[  x <= (5*l) | x > ( 5*(l+1) ) ] <- NA
  
  return(x)

}





## PLOT THEMES, COLOURS ETC ##

scale_colour_brewer( type = "seq", palette = "Oranges" )


## set up cluster for parallel computation


CapPerReg <- array( data=NA, dim= c( length(get.country( dir.europe) ),  length(Seasons), length(SlicesPerDay) ) )
OutputTable_1 <- NULL
OutputTable_2 <- NULL

MetaData<-NULL
listCap<-NULL 
Cap<-NULL
CF<-NULL

## begin loop ##

ptime <- system.time(
  {  
    for( i in 1:length( dir.europe ) ){
             
               for( j in 1:length(Seasons) ){
                    
                    for( k in 1:3){
                          
                            
                            ## create raster of CF maps / i (region) / j (timeslice)
                            
                            ras.CF <- raster( paste0( dir.out.GTiff, "\\CF_Overlay_Final\\", 
                                                      "CFout.", get.country( dir.europe )[i],".", 
                                                      Seasons[j],".", 
                                                      SlicesPerDay[k],".tif" 
                                                    ) 
                                            )
                            
                            ## create raster with val= cell area in km^2                     
                            
                            CellArea <- area( ras.CF, na.rm= TRUE )
                            
                            ## Calculate electricity generation per km^2 [MWh/km^2]
                            
                            GenDens <- calc( ras.CF, 
                                             fun= function(x){ x * NomGenDens })
                            
                            ## Calculate capacity in each raster cell [MW]
                            
                            CapPerCell <- calc( CellArea, fun= function(x){x * PowerDensity} )
                            
                            
                            ## SAVE cell capacity to .CSV and ggplot it ##
                            
                            MetaData <- rbind( MetaData, 
                                               data.frame( Country = get.country( dir.europe )[i],
                                                           Season = Season[j],
                                                           TimeSlice = SlicesPerDay[k] 
                                                         )
                                             ) 
                            
                            listCap <- cbind(  listCap, 
                                               data.frame(  Cap = ( sort( getValues(CapPerCell), 
                                                                    decreasing= FALSE, 
                                                                    na.last= NA) ),
                                                            CF =  ( sort( getValues(ras.CF), 
                                                                    decreasing= FALSE, 
                                                                    na.last= NA) )
                                                          )
                                            )
                          
                            ## Calculate the capacity in each region and timeslice
                            
                            CapPerReg <- cellStats( CapPerCell, "sum" )



                            ## Capacity statistics

                            MaxCap <- maxValue( CapPerCell )
                            MinCap <- minValue( CapPerCell )
                            CapAvg <- cellStats( CapPerReg, "mean" )

                            CapArea <- cellStats( CellArea, 'sum' )


                            ## Record outputs in table:

                            OutputTable_2 <- rbind( OutputTable_2,
                                                    data.frame( Region= get.country( dir.europe )[i],
                                                                Max= MaxCap,
                                                                Min= MinCap,
                                                                MeanCF= CapAvg,
                                                                CountryCap= CapPerReg,
                                                                AreaPot= CapArea,
                                                                check.names=T ) )
                
                                        
                                                ## BEGIN FOREACH PARALLEL CLUSTER ##
                                        
                                                  
                                                for( l in 1:length( n.CFs )){
              
                                                 
                                                            
                              
                                                
                                                 ## Calculate the capacity in each CF band
                                                 
                                                 CF.mask <- mask( CapPerCell, CFfuncout <- CF.func( ras.CF ) )
                                                 
                                                 CapPerCFband <- cellStats( CF.mask, 'sum' )
            
                                                 ## Record outputs in table:
                                                 
                                                 OutputTable_1 <- cbind( OutputTable_1, 
                                                                         data.frame( CF= CapPerCFband, 
                                                                                     check.names= T ) )
                                                 
                                                }
                          
                          
                          
                                    }
                         }
                }
  })#stopCluster(cl) ##system.time 





 


colnames( OutputTable_1 ) <- c( "ADM0_A1",
                                "Season",
                                "hourly Slice",
                                "CF1-10 [MW]" )


colnames( OutputTable_2 ) <- c( "ADM0_A1",
                                "MaxCap",
                                "MinCap",
                                "MeanCap",
                                "Country",
                                "Area Potential [km^2]",
                                "Season / hourly Slice",
                                )

write.csv( OutputTable_1, file = paste0( dir.out.stats, "\\OnshoreCapacityPerCF.csv" ))

write.csv( OutputTable_2, file = paste0( dir.out.stats, "\\OnshoreCapacityCountry.csv" ), qmethod = "double")


