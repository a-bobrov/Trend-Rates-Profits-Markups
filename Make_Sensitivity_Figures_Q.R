# **************************************
# Author: Anton Bobrov
# bobrov@umich.edu
# Sensitivity Paper FRED VERSION Quarterly
# Last modified: 07/03/2024
# **************************************
rm(list = ls())
require(tidyverse)
require(scales)
require(fredr)
require(zoo)
require(roll)
require(lubridate)
theme_set(theme_minimal(12))

# **************************************
wdir <- "C:/Users/anton/Documents/Coding/Research/Trends Sensitvity/Econ Bulletin-AEL" #Set this to the location of this file
api_key <- "a7a81cdd8e51d9680757e3935cb62215" #Get API key here: https://fred.stlouisfed.org/docs/api/api_key.html

# **************************************
setwd(wdir)
source("Helper_Functions_Q.R")
fredr_set_key(api_key)

data.FRED <- load.data.FRED()
Barkai <- load.data.Barkai()
master <- gen.vars(data.FRED)
generate.figures()


