library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)


# data
data1879_1895 <- "Data1879_1895.xlsx"
data1908_1918 <- "Data1908_1918.xlsx"
data2010_2020 <- "Data2010_2020.csv"
bezirk2010 <- "Bezirksnummern.xlsx"
population1860_1920  <- "Population_district_1860_2008.xlsx" 
population2014_2020  <- "Population_district_2014_2020.xlsx" 

# load functions
source("R/prepare_data.R")
