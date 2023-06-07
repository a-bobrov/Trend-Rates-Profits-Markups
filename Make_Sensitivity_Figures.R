# **************************************
# Author: Anton Bobrov
# Anton.Bobrov@sf.frb.org
# Sensitivity Paper FRED VERSION Main
# Last modified: 06/06/2022
# **************************************
rm(list = ls())
require(dplyr)
require(fredr)
require(ggthemes)
require(ggplot2)
require(zoo)
theme_set(theme_minimal(6))

# **************************************
wdir <- "" #Set this to the location of this file
api_key <- "" #Get API key here: https://fred.stlouisfed.org/docs/api/api_key.html

# **************************************
setwd(wdir)
source("Helper_Functions.R")
fredr_set_key(api_key)

data.FRED <- load.data.FRED()
Barkai <- load.data.Barkai()
master <- gen.vars(data.FRED)
generate.figures()
