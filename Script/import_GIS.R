# Import shapefile IRIS ----

IRIS_DATA <- st_read("Raw/IRIS_DATA/IRIS_DATA_ESRI.shp") 
# CRS : Lambert-93 = 2154
# plot(IRIS_DATA["X_revenus_m"])
