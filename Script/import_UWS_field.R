# Import UWS field ----

uws_field <- read_excel(path = "Raw/field_UWS.xlsx",
                        sheet = "Data_field",
                        skip = 1)

names(uws_field) = column_names$Column_corr


## create unique id ----
uws_field <- uws_field %>%
  mutate( ID_site = paste(toupper(str_sub(Arr, 1,3)), Sites, sep = "_"))

## Extract coordinates into two columns ----
uws_field <- separate (uws_field, col = Coords, into = c("Latitude", "Longitude"), sep = ",")

#WE MAY NEED TO DO THIS AS WELL ?
# UWS_Paris_sf <- st_as_sf(UWS_Paris, coords = c("Longitude", "Latitude"), crs = st_crs(4326)) #the original crs is 4326 !!
# UWS_Paris_sf <- st_transform(UWS_Paris_sf, crs =  2154)