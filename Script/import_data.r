# Import data

# Load packages ----

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(sf)

# Import UWS ----


## import column names ----

column_names = read_excel(path = "Raw/column_names.xlsx")


## import csv by district ----
Ivry = read_excel(path = "Raw/260218_UWS.xlsx",
           sheet = "Ivry",
           skip = 1)
names(Ivry) = column_names$Column_corr


Neuilly = read_excel(path = "Raw/260218_UWS.xlsx",
                  sheet = "Neuilly",
                  skip = 1)
names(Neuilly) = column_names$Column_corr

Seven = read_excel(path = "Raw/260218_UWS.xlsx",
                     sheet = "7e",
                     skip = 1)
names(Seven) = column_names$Column_corr

Nineteen = read_excel(path = "Raw/260218_UWS.xlsx",
                     sheet = "19e",
                     skip = 1)
names(Nineteen) = column_names$Column_corr

Fifth = read_excel(path = "Raw/260218_UWS.xlsx",
                      sheet = "5e",
                      skip = 1)
names(Fifth) = column_names$Column_corr

## bind all data tables together: ----
UWS_Paris <- rbind(Ivry,Neuilly,Seven,Nineteen,Fifth)



## create unique id ----
UWS_Paris <- UWS_Paris %>%
  mutate( ID_site = paste(toupper(str_sub(Arr, 1,3)), Sites, sep = "_"))



## Extract coordinates into two columns ----
UWS_Paris <- separate (UWS_Paris, col = Coords, into = c("Latitude", "Longitude"), sep = ",")
UWS_Paris_sf <- st_as_sf(UWS_Paris, coords = c("Longitude", "Latitude"), crs = st_crs(4326)) #the original crs is 4326 !!
UWS_Paris_sf <- st_transform(UWS_Paris_sf, crs =  2154)
plot(UWS_Paris_sf["Total wildness"])

# st_crs(UWS_Paris_sf)

# Temporary : Import IRIS data as XLS ----
UWS_iris = read_excel(path = "Raw/UWS_iris.xlsx",
                      sheet = "V1_iris_réattribués",
                      na = c("","NA"))

# Import shapefile IRIS ----
IRIS_DATA <- st_read("Raw/IRIS_DATA/IRIS_DATA_ESRI.shp") 
# CRS : Lambert-93 = 2154
plot(IRIS_DATA["X_revenus_m"])


# TO DO Match IRIS data with UWS data ----

uws_paris_iris <- st_join(UWS_Paris_sf,
                          IRIS_DATA,
                          left = TRUE)
plot(uws_paris_iris["X_revenus_m"])
plot(uws_paris_iris["Total wildness"])

ggplot()+
  geom_sf(data = IRIS_DATA, fill = "red", color = "blue")+
  geom_sf(data = UWS_Paris_sf , aes(color = "UWS_subj"), size =3)



# Export clean data (csv and/or .Rdata) ----

