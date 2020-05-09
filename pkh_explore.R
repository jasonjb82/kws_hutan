# load required libraries -------------------------------------------------
library(rgdal)
library(sf)
library(tidyverse)
library(jsonlite)
library(aws.s3)
library(scales)
library(measurements)

# set dropbox directory output --------------------------------------------
file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\leakage\\Data\\klhk\\pkh\\")

# credentials -------------------------------------------------------------
aws.signature::use_credentials()
bucket <- "trase-indonesia"

# read data ---------------------------------------------------------------
# pelepasan
pkh <- readOGR(paste0(wdir,"Pelepasan_Kawasan_Hutan.shp"))
# boundaries
obj <- get_object("6_BOUNDARIES/BIG/OUT/kabupaten_boundaries_2016_simplified.geojson", bucket)
kab <- read_sf(rawToChar(obj))

# select kab location details to add
kab_slim <- select(kab, prov_code, kab_code, kab, prov)

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

# province boundaries
prov_slim <- kab_slim %>%                  
  group_by(prov, prov_code) %>%
  summarize(geometry = st_union(geometry))

# cleaning up data ---------------------------------------------------------
pkh_df <- pkh %>%
  st_as_sf() %>%
  st_drop_geometry() %>% 
  mutate(DATE = lubridate::ymd(TGLSKPLS, truncated = 2L)) %>%
  #mutate(YEAR = str_extract(TGLSKPLS, "[^/]+")) %>%
  mutate(YEAR = lubridate::year(DATE)) %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV))

# calculate no of forest estate release by year for palm
no_pkh_palm_yr <- pkh_df %>%
  filter(str_detect(KOMODITAS, 'SAWIT')) %>%
  filter(!str_detect(KOMODITAS, 'NON')) %>%
  left_join(islands,by=c("KODEPROV"="prov_code")) %>%
  group_by(YEAR,KODEPROV,island) %>%
  count()

# calculate no of forest estate release by year for others
no_pkh_others_yr <- pkh_df %>%
  filter(!str_detect(KOMODITAS, 'SAWIT')) %>%
  left_join(islands,by=c("KODEPROV"="prov_code")) %>%
  group_by(YEAR,KODEPROV,island) %>%
  count()

no_pkh_non_palm_yr <- pkh_df %>%
  filter(str_detect(KOMODITAS, 'NON SAWIT')) %>%
  left_join(islands,by=c("KODEPROV"="prov_code")) %>%
  group_by(YEAR,KODEPROV,island) %>%
  count()

no_pkh_others_yr <- no_pkh_others_yr %>%
  bind_rows(no_pkh_non_palm_yr)

# plot ----------------------------------------------------------------
# set up theme
theme_plot <-theme(panel.background = element_rect(colour="grey30",fill=NA),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.title = element_text(hjust = 0.5),
                   axis.line = element_line(color ="grey30"),
                   axis.ticks.x = element_line(colour = "grey30"),
                   axis.ticks.y = element_line(colour = "grey30"),
                   axis.text.x = element_text(size = 9, color = "grey30",angle = 45,hjust=1),
                   axis.text.y = element_text(size = 9, color = "grey30"),
                   axis.title.x = element_text(size = 10, color = "grey30"),
                   axis.title.y = element_text(size = 10, color = "grey30"),
                   strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                   strip.background = element_rect(color=NA, fill=NA),
                   legend.key.height = unit(12, "pt"),
                   legend.key.width = unit(12, "pt"),
                   legend.text = element_text(size = 9,colour="grey30"),
                   legend.title = element_blank(),
                   legend.position="bottom",
                   legend.box="horizontal",
                   plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))


# plot of forest estate releases by year and island
# The palette with grey:
cbp1 <- c("#999999", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")

p1 <- ggplot(data = no_pkh_palm_yr) + geom_col(mapping = aes(x = YEAR, y = n,fill=island),alpha=0.85) +
  scale_y_continuous(expand = c(0,0),limits=c(0,75)) +
  ylab("No of forest estate releases for OP\n") +
  xlab("\nYear") +
  scale_fill_manual(values = cbp1) +
  scale_x_continuous(expand=c(0,0),breaks=seq(1987,2019,by=1))+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y",expand = c(0,0)) +
  theme_plot

p1

ggsave(p1,file="pkh_klkh_plot.png", dpi=400, w=9, h=5,type="cairo-png")

# Calculate area of PO releases

# choose projection: Cylindrical Equal Area
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# area for all of Indonesia
pkh_po_idn_area_df <- pkh %>%
  st_as_sf() %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  filter(str_detect(KOMODITAS, 'SAWIT')) %>%
  filter(!str_detect(KOMODITAS, 'NON')) %>%
  st_transform(crs=indonesian_crs) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  as.data.frame %>%
  mutate(DATE = lubridate::ymd(TGLSKPLS, truncated = 2L)) %>%
  mutate(YEAR = lubridate::year(DATE)) %>%
  ungroup() %>%
  select(-geometry) %>%
  left_join(islands,by=c("KODEPROV" ="prov_code")) %>%
  group_by() %>%
  summarize(area_ha = sum(conv_unit(area_m2,from="m2",to="hectare")))


# area by islands
pkh_po_island_area_df <- pkh %>%
  st_as_sf() %>%
  filter(str_detect(KOMODITAS, 'SAWIT')) %>%
  filter(!str_detect(KOMODITAS, 'NON')) %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  st_transform(crs=indonesian_crs) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  as.data.frame %>%
  mutate(DATE = lubridate::ymd(TGLSKPLS, truncated = 2L)) %>%
  mutate(YEAR = lubridate::year(DATE)) %>%
  ungroup() %>%
  select(-geometry) %>%
  left_join(islands,by=c("KODEPROV" ="prov_code")) %>%
  group_by(island) %>%
  summarize(area_ha = sum(conv_unit(area_m2,from="m2",to="hectare"))) %>%
  arrange(-area_ha) 

# releases by year
pkh_po_year_area_df <- pkh %>%
  st_as_sf() %>%
  filter(str_detect(KOMODITAS, 'SAWIT')) %>%
  filter(!str_detect(KOMODITAS, 'NON')) %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  st_transform(crs=indonesian_crs) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  as.data.frame %>%
  mutate(DATE = lubridate::ymd(TGLSKPLS, truncated = 2L)) %>%
  mutate(YEAR = lubridate::year(DATE)) %>%
  ungroup() %>%
  select(-geometry) %>%
  left_join(islands,by=c("KODEPROV" ="prov_code")) %>%
  group_by(YEAR) %>%
  summarize(area_ha = sum(conv_unit(area_m2,from="m2",to="hectare"))) %>%
  arrange(-area_ha) 

# releases by island by year
pkh_po_island_year_area_df <- pkh %>%
  st_as_sf() %>%
  filter(str_detect(KOMODITAS, 'SAWIT')) %>%
  filter(!str_detect(KOMODITAS, 'NON')) %>%
  mutate(KODEPROV = ifelse(KODEPROV == 92,94,KODEPROV)) %>%
  st_transform(crs=indonesian_crs) %>%
  mutate(area_m2 = st_area(.)) %>%
  mutate_at(vars(area_m2), units::drop_units) %>%
  as.data.frame %>%
  mutate(DATE = lubridate::ymd(TGLSKPLS, truncated = 2L)) %>%
  mutate(YEAR = lubridate::year(DATE)) %>%
  ungroup() %>%
  select(-geometry) %>%
  left_join(islands,by=c("KODEPROV" ="prov_code")) %>%
  group_by(island,YEAR) %>%
  summarize(area_ha = sum(conv_unit(area_m2,from="m2",to="hectare"))) %>%
  arrange(-area_ha) 


# plot of of forest estate release areas by island by year
p2 <- ggplot(data = pkh_po_island_year_area_df) + geom_col(mapping = aes(x = YEAR, y = round(area_ha,3),fill=island),alpha=0.85) +
  scale_y_continuous(expand = c(0,0),limits=c(0,600000),labels = unit_format(unit = "Mha", scale = 1e-6,trim=TRUE)) +
  ylab("Area of forest estate releases for OP\n") +
  xlab("\nYear") +
  scale_fill_manual(values = cbp1) +
  scale_x_continuous(expand=c(0,0),breaks=seq(1987,2019,by=1))+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y",expand = c(0,0)) +
  theme_plot

p2

ggsave(p2,file="pkh_klkh_area_plot.png", dpi=400, w=9, h=5,type="cairo-png")
