## ---------------------------
##
## Purpose of script: Getting information on KH 2019 and forest releases data for each province
##
## Author: Jason Benedict
##
## Date Created: 2020-05-12
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
wdir <- paste0(dropbox_dir,"\\KH\\")
setwd(wdir)

## read data -----------------
kh_pkh <- read_sf(paste0(wdir,"Analysis\\pkh_kh_union.shp"))
kab <- read_sf(paste0(wdir,"Data\\Indonesia\\big\\kabupaten_boundaries_2016.shp"))

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


# summarize data
kh_pkh_summ <- kh_pkh %>%
  select(KODEPROV,year,FUNGSIKWS) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  st_drop_geometry() %>%
  mutate(area_ha = area_m2/10000) %>%
  mutate(area_type = ifelse(FUNGSIKWS == 1007,"APL","KH")) %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  left_join(kab_slim,by=c("KODEPROV"="prov_code")) %>%
  group_by(prov,year,area_type) %>%
  summarize(total_area_ha = sum(area_ha)) %>%
  pivot_wider(names_from=area_type,values_from = total_area_ha,values_fill = list(total_area_ha = 0)) %>%
  filter(year != 0) %>%
  arrange(prov,year) %>%
  mutate(pc_kh = KH/(KH+APL)*100) %>%
  drop_na(prov)

# export to csv
write_excel_csv(kh_pkh_summ,paste0(wdir,"Analysis/pkh_kh_prov_pc_areas.csv"))
  
