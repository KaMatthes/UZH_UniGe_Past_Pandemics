.libPaths(c("C:/Users/kmatth/AppData/Local/R/win-library/4.2", "C:/Program Files/R/R-4.2.2/library"))

library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
# library(ggplot2)
library(zoo)
library(foreach)
library(arsenal)
library(rmarkdown)
library(kableExtra)
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(ggpattern)
library(INLA)
library(maptools)
library(colorspace)
library(viridis)
library(RColorBrewer)
library(mgcv)
library(cowplot)
library(introdataviz)
library(conflicted)
library(rgeoda)
library(MASS)
# library(extrafont)
library(raster)
loadfonts(quiet = T)

#
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("rbind.fill", "plyr")
conflict_prefer("group_by", "dplyr")



# data
data1879_1895 <- "Data1879_1895.xlsx"
data1908_1918 <- "Data1908_1918.xlsx"
data2010_2020 <- "Data2010_2020.csv"
bezirk2010 <- "Bezirksnummern.xlsx"
population1860_1920  <- "Population_district_1860_2008.xlsx" 
population2014_2020  <- "Population_district_2014_2020.xlsx" 


mort_pop <- 10000
no_classes_map <- 5
col_5_groups1 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
col_5_groups2 <- c("#ca0020","#f4a582","#ffffbf","#abd9e9","#2c7bb6")
col_5_groups2_trans <- c("#2c7bb6","#abd9e9","#ffffbf","#f4a582","#ca0020")
col_5_groups_green_trans <- c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c")
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

col_11_groups_green_trans <- c("#006d2c","#31a354","#74c476","#bae4b3","#edf8e9",
                               "#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")

coldiv <- diverge_hcl(8)  
col5magma <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "magma")
# load functions
col4viridis <- viridis(4, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col5viridis <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col8viridis <- viridis(8, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
cols_reds <- brewer.pal(n =8, name = "Reds")
cols_reds <- brewer.pal(n = 9, name = "Blues")

col_line <- "grey40"
lwd_size_points  <- 3
lwd_size <- 1.5
axix_size_legend <- 6
axis_size  <- 15
axis_size_title  <- 15
legend_size <- 15
legend_size_title <- 15
size_title <- 15
legend_size_map <-1
panel_size_map <- 1.5
legend_size_title_map <- 1.5


strip_text <- 20
size_axis_title <- 20
legend_size <- 15
legend_size_map <- 1
legend_size_title <- 1
main_size_map <- 1.5


strip_text <- 25
lwd_size <- 2
pch_type <- 19
lwdline <- 1
size_legend <- 15
size_legend_title<- 15
pd <-position_dodge(width=0.3)
fatten_size <- 8
plot_title <- 25

size_axis <- 22
size_axis_title <- 22
size_plot_title <-15
size_legend_text <- 20


normalit <- function(m){
  (m - min(m))/(max(m)-min(m))
}
# source("R/prepare_data.R")
# source("R/population.R")
# source("R/mortality_rate.R")

# maps_swiss <-  read_sf("data_raw/Map_Relief/g2k15.shp") 
# relief <- raster("data_raw/Map_Relief/02-relief-ascii.asc") %>%
#   mask(maps_swiss) 

data_canton <- read_sf("data_raw/Map_Canton/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") 

data_centroiod <-st_as_sf(read_sf("data_raw/Map_data_1918/centroids.shp")) %>%
  filter(District == "Aarau"  | District == "Appenzell IR" 
         | District == "Basel"  |  District == "Bellinzona" 
         | District == "Bern" |  District == "Delemont" 
         |  District == "Frauenfeld"  |  District == "Geneve"
         |  District == "Glarus"  |  District == "Lausanne"
         |  District == "Liesthal"  |  District == "Luzern"
         |  District == "Neuchatel" |  District == "Nidwalden"
         |  District == "Obwalden"  |  District == "Plessur"
         |  District == "Sarine"  |  District == "Schaffhausen"
         |  District == "Schwyz"  |  District == "Sion"
         |  District == "Solothurn-Lebern" |  District == "St. Gallen"
         |  District == "Uri"  |  District == "Zug"
         |  District == "Zürich")

source("R/PerYear.R")
source("R/PerYear_sex.R")
source("R/PerYear_age.R")
# source("R/maps.R")
source("R/maps_inla.R")
source("R/maps_sex_inla.R")
source("R/maps_age_inla.R")
source("R/maps_age_inla.R")
source("R/maps_age_1918_inla.R")
# source("R/maps_age_sex_inla.R")
source("R/maps_age_sex_1918_inla.R")
source("R/maps_age_1918.R")
source("R/maps_1890_1918.R")
source("R/maps_age_1918_1890.R")
source("R/maps_sep.R")
source("R/maps_tbc.R")

    
    source("R/cor_child_mort.R")
    source("R/cor_schoolkids.R")
    source("R/cor_hospitals.R")
    source("R/cor_gdp.R")
    source("R/cor_sep.R")
    source("R/cor_urbanity.R")
    source("R/cor_tbc.R")
    source("R/cor_area.R")
    source("R/cor_stations.R")
    source("R/cor_1890_1918.R")
    source("R/cor_age30_1918.R")
    source("R/cor_kfo_index.R")
    source("R/Regression.R")
    source("R/Hotspots.R")
    source("R/Hotspots_sex.R")
    source("R/Regression_model.R")
    
    
    
    
    # render html
    # render(paste0("R/PastPandemics_Excess_Mortality_inla.Rmd"), output_file = paste0("../output/",today(),"_Report_PastPandemice_inla.html"))
    
    render(paste0("R/PastPandemics_Excess_Mortality_cofactors.Rmd"), output_file = paste0("../output/",today(),"_Report_PastPandemice_cofactors.html"))