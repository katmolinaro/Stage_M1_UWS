#Master script

# load libraries ----

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(sf)


# import and clean data ----

## import GIS data ----

source("Script/import_GIS.R")

## Google UWS ----

source("Script/import_UWS_google.r")

## Field UWS ----

source("Script/import_vegetation.R")

## Field vegetation surveys ----

# Transform data ----

## UWS google ----
## UWS terrain ----

## Calculate Richness ----

source("Script/richness.R")

# Analyses ----

