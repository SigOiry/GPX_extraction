library(terra)
library(rstudioapi)


library(terra)

# Define the paths for your Sentinel-2 bands
# Assuming band files are named in a way that allows easy identification by resolution
# Example: "B02_10m.tif", "B03_10m.tif", "B04_10m.tif", "B05_20m.tif", etc.

img_path <- selectDirectory()

band_files <- list.files(img_path, pattern = "\\.jp2$", full.names = TRUE, recursive = T) %>% 
  as_tibble() %>% 
  rename(path = "value") %>% 
  mutate(resolution = case_when(grepl("10m", path) ~ 10,
                                grepl("20m", path) ~ 20,
                                grepl("60m", path) ~ 60),
         bands = case_when(grepl("B01", path) ~ "B01",
                           grepl("B02", path) ~ "B02",
                           grepl("B03", path) ~ "B03",
                           grepl("B04", path) ~ "B04",
                           grepl("B05", path) ~ "B05",
                           grepl("B06", path) ~ "B06",
                           grepl("B07", path) ~ "B07",
                           grepl("B08", path) ~ "B08",
                           grepl("B09", path) ~ "B09",
                           grepl("B10", path) ~ "B10",
                           grepl("B11", path) ~ "B11",
                           grepl("B12", path) ~ "B12",
                           grepl("B8A", path) ~ "B08A")) %>% 
  dplyr::filter(!is.na(resolution),
                !is.na(bands)) %>% 
  group_by(bands) %>% 
  dplyr::filter(resolution == min(resolution)) %>% 
  arrange(bands)

rast_temp <- band_files %>% 
  dplyr::filter(resolution == 10) %>% 
  head(1) %>%
  pull(path) %>% 
  rast()

for(i in 1:nrow(band_files)){
  print(i)
  img <- band_files$path[i] %>% 
    rast()
  if(band_files$resolution[i] != 10){
    img <- resample(img,rast_temp)
    }
  if(i == 1){
    stk <- img
  }else{
    stk <- rast(list(stk,img))
  }
}

writeRaster(stk,"S2_19092024.tif")

