##########################################################
###########################################################
##### Calculo de Variables bios de ETP
###########################################################
###########################################################

### Agosto 2016 - Castro & Mesa

library(gtools)
library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(usdm)
library(maps)
library(SDMTools)
library(maptools)
library(stringr)

## Librerias para trabajar en paralelo
library(foreach)
library(doSNOW) 


### Lineas a Modificar

path <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc/"  ## path raster evapatransporation

output <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)

path_models <- list.dirs(path, recursive = F)
models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models

#### 

## Librerias para trabajar en paralelo
library(foreach)
library(doSNOW) 

cl <- makeCluster(10)
registerDoSNOW(cl)  ## For Windows


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_1') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_1(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_1.asc'))
    
  }
  
}


close(pb)


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_1') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_2(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_2.asc'))
    
  }
  
}


close(pb)




length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_1') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_3(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_3.asc'))
    
  }
  
}




close(pb)



length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_1') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_4(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_4.asc'))
    
  }
  
}




close(pb)


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_1') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_5(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_5.asc'))
    
  }
  
}




close(pb)
stopCluster(cl)










######################

plot(etp_layer_st)

path_var <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s"
path_var <- list.dirs(path_var, recursive = F)
path_var <- lapply(path_var, list.dirs, recursive = F) ## [[1]] numero modelo [[]][1] numero de año


# a <- path_var[[1]][1]
# a <- list.files(a, full.names = T, pattern = ".asc$")
# a[1]

path_files <- lapply(path_var, list.files, full.names = T, pattern = ".asc$")

for(j in 1:2){ #anio

  for (i in 1:length(path_files)){ #modelo
    
    listado <- list.files(path_var[[i]][j], full.names = T, pattern = ".asc$")
    
    path_etp <- mixedsort(subset(y, str_detect(y,"etp")))
    path_prec <- mixedsort(subset(listado, str_detect(listado,"prec")))
    
    etp <- lapply(path_etp, FUN = raster) %>%
      stack()
    prec  <- lapply(path_prec, FUN = raster) %>%
      stack()
    
    rlist <- prec
    rlist2 <- etp
    
    outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP8.asc'
    format <- 'ascii'
    
    ETP_6()
  }
  
}
path_prec <- subset(path_files, str_detect(path_var,"prec"))
  
  
prec <- paste0(path_prec, "/prec_", 1:12, ".asc")
prec <- lapply(prec, FUN = raster) 

path_tmean <- "D:/CC/_bd/_colombia/_raster/_worldclim_v1/_30s/_asc"
tmean <- paste0(path_tmean, "/tmean_", 1:12, ".asc")
tmean <- lapply(tmean, FUN = raster) 

######################




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

## Función para ejecutar la variable ETP 6 que es el rango (ETP Max - ETP Min)

for(i in 1:length(year)){
  
  cat('year ', year[i], '\n')
  
  for(m in 1:length(models)){
    
    cat('model ', models[m], '\n')
    
    output_all <- paste0(output, '/', models[m],  '/', year[i], '/')
    
    ETP_8(r, filename = paste0(output_all, '/ETP_5.asc'))
    
  }
}


outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP8.asc'

ETP8Calc(prec, etp_layers, outfile, format = 'ascii')

