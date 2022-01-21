library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)

# data
data1879_1895 <- "Data1879_1895.xlsx"
data1908_1918 <- "Data1908_1918.xlsx"
data2010_2020 <- "Data2010_2020.csv"
bezirk2010 <- "Bezirksnummern.xlsx"

# load functions
source("R/prepare_data.R")
