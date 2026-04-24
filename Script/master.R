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

## Rescoring

source("Script/import_rescoring.R")

# Transform data ----

## Calculate indices (richness, etc.) for each transect ----
source("Script/transect summary.R")


# Analyses ----

# source("Script/exploration.r")
# source("Script/analyses_field.R")
# source("Script/uws_google_field_analysis.R")


