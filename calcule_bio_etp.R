##########################################################
###########################################################
##### Calculo de Variables bios de ETP
###########################################################
###########################################################

### Agosto 2016 - Mesa & Castro

require(gtools); require(rgdal); require(sp); require(raster); require(dplyr); require(usdm); require(maps); library(SDMTools); library(maptools)
require(dplyr)

path <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc"  ## path raster evapatransporation

output <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)


######################

plot(etp_layer_st)

path_prec <- "D:/CC/_bd/_colombia/_raster/_worldclim_v1/_30s/_asc"
prec <- paste0(path_prec, "/prec_", 1:12, ".asc")
prec <- lapply(prec, FUN = raster) 

path_tmean <- "D:/CC/_bd/_colombia/_raster/_worldclim_v1/_30s/_asc"
tmean <- paste0(path_tmean, "/tmean_", 1:12, ".asc")
tmean <- lapply(tmean, FUN = raster) 

######################

x <- list.dirs(path, recursive = F)
models <- basename(x)
year <- c('2020_2049', '2040_2069')

y <- lapply(x, list.dirs, recursive = F)
names(y) <- models


z <- list.files(y[[models[1]]][1], full.names = T, pattern = '.asc$') ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069

w <- mixedsort(z) #ordenar de menor a mayor

r <- lapply(w, raster)  %>%
  stack()



## Función para ejecutar la variable ETP 1 que es la evapotranspiración acumulada

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
  
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_1(r, filename = paste0(output_all, '/ETP_1.asc'))
    
  }
}

## Función para ejecutar la variable ETP 2 que es la estacionalidad de la evapotranspiración (coeficiente de variación x 100)

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
    
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_2(r, filename = paste0(output_all, '/ETP_2.asc'))
    
  }
}

## Función para ejecutar la variable ETP 3 que es la ETP máxima de los 12 meses del año

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
    
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_3(r, filename = paste0(output_all, '/ETP_3.asc'))
    
  }
}

## Función para ejecutar la variable ETP 4 que es la ETP mínima de los 12 meses del año

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
    
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_4(r, filename = paste0(output_all, '/ETP_4.asc'))
    
  }
}

## Función para ejecutar la variable ETP 5 que es el rango (ETP Max - ETP Min)

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
    
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_5(r, filename = paste0(output_all, '/ETP_5.asc'))
    
  }
}




outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP8.asc'

ETP8Calc(prec, etp_layers, outfile, format = 'ascii')

