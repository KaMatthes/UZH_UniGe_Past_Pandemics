.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.1.2/library"))

library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(rgdal)
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


col_11_groups_green_trans <- c("#006d2c","#31a354","#74c476","#bae4b3","#edf8e9",
                               "#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")

coldiv <- diverge_hcl(8)  
col5magma <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "magma")
# load functions
col5viridis <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col8viridis <- viridis(8, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
cols_reds <- brewer.pal(n =8, name = "Reds")
cols_reds <- brewer.pal(n = 9, name = "Blues")
# source("R/prepare_data.R")
# source("R/population.R")
# source("R/mortality_rate.R")

source("R/PerYear.R")
source("R/PerYear_sex.R")
source("R/PerYear_age.R")
# source("R/maps.R")
source("R/maps_inla.R")
source("R/maps_sex_inla.R")
source("R/maps_age_inla.R")
source("R/maps_age_inla.R")
source("R/maps_age_1918_inla.R")
source("R/maps_age_sex_inla.R")
source("R/maps_age_sex_1918_inla.R")

# source("R/maps_age2groups_sex.R")
# source("R/lisa.R")
# source("R/lisa_sex.R")
# source("R/lisa_age.R")
# source("R/lisa_age_sex.R")
# functios
# function_prepare_data()
# function_population()
# function_mortality_rate()


# render html
# render(paste0("R/PastPandemics_Excess_Mortality_inla.Rmd"), output_file = paste0("../output/",today(),"_Report_PastPandemice_inla.html"))

render(paste0("R/PastPandemics_Excess_Mortality_cofactors.Rmd"), output_file = paste0("../output/",today(),"_Report_PastPandemice_cofactors.html"))