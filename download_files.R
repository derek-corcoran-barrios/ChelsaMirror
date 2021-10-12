library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(terra)


#Carpeta de capas

Table_Layers <- read_csv("envidatS3paths.txt", col_names = FALSE) %>% 
  magrittr::set_names("Link") %>% 
  mutate(model = str_remove_all(Link, "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/cmip5/2061-2080/bio/CHELSA_bio_mon_")) %>% 
  tidyr::separate(model, sep = "_", into = c("Model", "RCP", "Coso", "Coso2", "Bio", "Year", "Version")) %>% 
  dplyr::select(Link, Model, RCP, Year, Bio)

Years <- unique(Table_Layers$Year)

dir.create(Years)

RCPs <- unique(Table_Layers$RCP)

Folders <- paste(Years, RCPs, sep = "/")


for(i in 1:length(Folders)){
  dir.create(Folders[i])
}



Layers <- Table_Layers %>% 
  mutate(Bio = as.numeric(Bio)) %>% 
  dplyr::group_split(Model, RCP, Year) %>% 
  purrr::map(~arrange(.x, Bio)) %>% 
  purrr::map(~mutate(.x, Bio = formatC(Bio, digits = 1, flag = "0", mode = "integer")))


AllFiles <- vector()

for(i in 1:length(Layers)){
  AllFiles[i] <- paste(unique(Layers[[i]]$Year),unique(Layers[[i]]$RCP), paste(unique(Layers[[i]]$Model), unique(Layers[[i]]$RCP),".tif", sep = "_"), sep = "/")
}

Cond <- !file.exists(AllFiles)

Layers <- Layers[Cond]


for(i in 1:length(Layers)){
  STACK <- list()
  for(j in 1:nrow(Layers[[i]])){
    while (length(STACK) < j) {
      try({
        STACK[[j]] <- terra::rast(paste0('/vsicurl/', Layers[[i]]$Link[j])) %>% 
          magrittr::set_names(paste0("bio", Layers[[i]]$Bio[j]))})
    }
    message(paste(j, "of", 19))
  }
  

  STACK <- rast(STACK)
  
  writeRaster(STACK, paste(unique(Layers[[i]]$Year),unique(Layers[[i]]$RCP), paste(unique(Layers[[i]]$Model), unique(Layers[[i]]$RCP),".tif", sep = "_"), sep = "/"), overwrite=TRUE)
  
  To_erase <- list.files(pattern = "Temp", full.names = T)
  
  file.remove(To_erase)
  print(paste(i, "of", length(Layers), "Ready!"))
}

### Current

library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(terra)


#Carpeta de capas

Table_Layers <- read_csv("Current.txt", col_names = FALSE) %>% 
  magrittr::set_names("Link") %>% 
  mutate(Bio = str_remove_all(Link, "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio/CHELSA_bio10_")) %>% 
  mutate(Bio = str_remove_all(Bio, ".tif"))

dir.create("Current")

Table_Layers <- Table_Layers[1:19,]

STACK <- list()
  for(i in 1:nrow(Table_Layers)){
    while (length(STACK) < i) {
      try({
        STACK[[i]] <- terra::rast(paste0('/vsicurl/', Table_Layers$Link[i])) %>% 
          magrittr::set_names(paste0("bio", Table_Layers$Bio[i]))})
    }
    message(paste(i, "of", 19))
  }
  
  
STACK <- rast(STACK)
  
writeRaster(STACK, paste("Current", "Bioclim.tif", sep = "/"), overwrite=TRUE)
  
  
