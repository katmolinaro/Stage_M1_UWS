#Master script

# load libraries ----

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(sf)
library(DHARMa)
library(lme4)
library(lmerTest)


# import and clean data ----

## import GIS data ----

source("Script/import_GIS.R")

## Google UWS ----

source("Script/import_UWS_google.r")

## Field UWS ----

source("Script/import_UWS_field.R")

## Field vegetation surveys ----

source("Script/import_vegetation.R")

# Transform data ----

## UWS google ----
## UWS terrain ----

## Calculate indices (richness, etc.) for each transect ----
source("Script/transect summary.R")


# Analyses ----

