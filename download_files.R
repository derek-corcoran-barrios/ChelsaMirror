library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(rgdal)

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


for(i in 1:length(Layers)){
  STACK <- list()
  for(j in 1:nrow(Layers[[i]])){
    while (length(STACK) < j) {
      try({download.file(Layers[[i]]$Link[j], destfile = paste0("Temp", Layers[[i]]$Bio[j], ".tif"))
        STACK[[j]] <- raster(paste0("Temp", Layers[[i]]$Bio[j], ".tif")) %>% 
          magrittr::set_names(paste0("bio", Layers[[i]]$Bio[j]))})
    }
    message(paste(j, "of", 19))
  }
  

  STACK <- STACK %>% reduce(stack)
  
  writeRaster(STACK, paste(unique(Layers[[i]]$Model), unique(Layers[[i]]$RCP),".tif", sep = "_"), overwrite=TRUE)
  
  To_erase <- list.files(pattern = "Temp", full.names = T)
  
  file.remove(To_erase)
  print(paste(i, "of", length(Layers), "Ready!"))
}

