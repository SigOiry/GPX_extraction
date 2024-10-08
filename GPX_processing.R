library(terra)
library(tidyverse)
library(sf)
library(rstudioapi)


folder_path <- rstudioapi::selectDirectory(caption = "Select a Directory") 

filelist <- folder_path %>% 
  list.files(pattern = ".gpx", recursive = T, full.names = T) %>% 
  as_tibble() %>% 
  rename(path ="value") %>% 
  mutate(filename = gsub(".*/","",path),
         GPS = str_remove(path,paste0("/",filename)) %>% gsub(".*/","",.),
         Class = case_when(str_detect(filename, "WALK") ~ "NA",
                           str_detect(filename, "MAC") ~ "Macroalgae",
                           str_detect(filename, "MA") ~ "Macroalgae",
                           str_detect(filename, "AL") ~ "Macroalgae",
                           str_detect(filename, "AG") ~ "Macroalgae",
                           str_detect(filename, "SG") ~ "Macroalgae",
                           str_detect(filename, "MI") ~ "Macroalgae",
                           str_detect(filename, "ÂLG") ~ "Macroalgae",
                           str_detect(filename, "SED") ~ "Sediment",
                           str_detect(filename, "SÈD") ~ "Sediment",
                           str_detect(filename, "MUD") ~ "Sediment",
                           str_detect(filename, "SE") ~ "Sediment",
                           str_detect(filename, "SD") ~ "Sediment",
                           str_detect(filename, "BS") ~ "Sediment",
                           str_detect(filename, "OY") ~ "Oyster",
                           str_detect(filename, "WAT") ~ "Water",
                           str_detect(filename, "WA") ~ "Water",
                           str_detect(filename, "W") ~ "Water",
                           str_detect(filename, "ANG") ~ "Angiosperm",
                           T ~ "NA")) %>% 
  dplyr::filter(Class %in% c("Macroalgae","Sediment","Oyster","Water") &
                  GPS != "LB1" & 
                  GPS != "Argentina")


for(i in 1:nrow(filelist)){
  waypoints <- st_read(filelist$path[i],layer = "track_points") %>% 
    dplyr::select(geometry)

  poly <- st_convex_hull(st_union(waypoints)) %>% 
    st_as_sf() %>% 
    mutate(class = filelist$Class[i],
           GPS = filelist$GPS[i],
           filename =filelist$filename[i]) %>% 
    rename(geometry= "x") %>% 
  st_buffer(0)
  
  if(i == 1){
    shp <- poly
  }else{
    shp <- rbind(shp,poly)
  }

}

Water <- shp %>% 
  dplyr::select(-c(GPS,filename)) %>% 
  dplyr::filter(class == "Water") %>%
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  mutate(class = "Water")

Sed <- shp %>% 
  dplyr::select(-c(GPS,filename)) %>% 
  dplyr::filter(class == "Sediment") %>%
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  mutate(class = "Sediment",
         classid = 1 %>%  as.integer())

Algae <- shp %>% 
  dplyr::select(-c(GPS,filename)) %>% 
  dplyr::filter(class == "Macroalgae") %>%
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  mutate(class = "Macroalgae",
         classid = 2 %>%  as.integer()) 

Oy <- shp %>% 
  dplyr::select(-c(GPS,filename)) %>% 
  dplyr::filter(class == "Oyster") %>%
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  mutate(class = "Oyster",
         classid = 3 %>%  as.integer())


merged_poly <- rbind(
  # Water,
  Sed,Algae,Oy)

a <- merged_poly %>% 
  # st_sf() %>% 
  # st_make_valid() %>% 
  st_intersection() %>% 
  filter(n.overlaps == 1) %>% 
  mutate(origins = as.numeric(origins)) %>% 
  # select(-origins) %>% 
  st_make_valid() %>% 
  st_buffer(0)


b <- a %>% 
  dplyr::filter(origins == 82) %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry) %>% as.numeric())
 


c <- a %>% 
  dplyr::filter(origins != 82)  %>% 
  mutate(area = st_area(geometry) %>% as.numeric()) %>% 
  rbind(b) %>% 
  dplyr::filter(area > 100) 
  





write_sf(merged_poly, "all_polygons.shp")
write_sf(c, "all_polygons_cleaned.shp")


