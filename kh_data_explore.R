## ---------------------------
##
## Purpose of script: Getting information on KH 2019 data for each province
##
## Author: Jason Benedict
##
## Date Created: 2020-05-08
## 
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load packages

library(tidyverse)
library(data.table)
library(janitor)
library(jsonlite)
library(lubridate)
library(sf)
library(sp)


## set working directory -----
file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\KH\\Data\\Indonesia\\")
setwd(wdir)

## read data -----------------
kh_2019 <- read_sf(paste0(wdir,"klhk\\kh_2019\\kawasan_hutan.shp"))
kab <- read_sf(paste0(wdir,"big\\kabupaten_boundaries_2016.shp"))

kab_slim <- kab %>%
  st_drop_geometry() %>%
  select(prov,prov_code) %>%
  distinct()

# add islands
islands <- kab %>%
  st_drop_geometry() %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "SUMATRA",island == 2 ~ "RIAU ARCHIPELAGO",
      island == 3 ~ "JAVA", island == 5 ~ "BALI ETC",
      island == 6 ~ "KALIMANTAN",island == 7 ~ "SULAWESI",
      island == 8 ~ "MALUKU",island == 9 ~ "PAPUA"
    )
  ) %>%
  distinct(prov_code,island)

## explore -------------------

# getting summarized information of SK year and numbers for each province
kh_2019_summ <- kh_2019 %>%
  select(KODEPROV,NOSKPNJK,TGLSKPNJK,KET) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  st_drop_geometry() %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  group_by(KODEPROV,NOSKPNJK,TGLSKPNJK,KET) %>%
  summarize(area_ha = sum(area_m2/100000)) %>%
  left_join(kab_slim,by=c("KODEPROV"="prov_code")) %>%
  left_join(islands,by=c("KODEPROV"="prov_code")) %>%
  distinct() %>%
  mutate(YEAR = year(TGLSKPNJK))

# export to xlsx
write_excel_csv(kh_2019_summ,paste0(wdir,"klhk\\kh_2019\\data_summary.csv"))
