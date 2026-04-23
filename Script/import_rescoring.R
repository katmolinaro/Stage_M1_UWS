#import data of the re-done uws google of the field points

rescoring = read_excel(path = "Raw/redo_uws_google.xlsx",
                  sheet = "19th",
                  skip = 1)
names(rescoring) = column_names$Column_corr

# create id

rescoring <- rescoring %>%
  mutate( ID_site = paste(toupper(str_sub(Arr, 1,3)), Sites, sep = "_"))

# extract coord

rescoring <- separate (rescoring, col = Coords, into = c("Latitude", "Longitude"), sep = ",")

#Spatial object
rescoring_iris <- st_as_sf(rescoring, coords = c("Longitude", "Latitude"), crs = st_crs(4326)) #the original crs is 4326 !!
rescoring_iris <- st_transform(rescoring_iris, crs =  2154)

# Match IRIS data with UWS data ----

rescoring_iris <- st_join(rescoring_iris,
                          IRIS_DATA,
                          left = TRUE)



