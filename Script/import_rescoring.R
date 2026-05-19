#import data of the re-done uws google of the field points

nin = read_excel(path = "Raw/redo_uws_google.xlsx",
                  sheet = "19th",
                  skip = 1)
names(nin) = column_names$Column_corr

sev = read_excel(path = "Raw/redo_uws_google.xlsx",
                 sheet = "7th",
                 skip = 1)
names(sev) = column_names$Column_corr

#bind the two sheets

rescoring <- rbind(nin, sev)

# create id

rescoring <- rescoring %>%
  mutate( ID_site = paste(toupper(str_sub(Arr, 1,3)), toupper(str_sub(Sites, 1,3)), sep = "_"))

# extract coord

rescoring <- separate (rescoring, col = Coords, into = c("Latitude", "Longitude"), sep = ",")

#Spatial object
rescoring_iris <- st_as_sf(rescoring, coords = c("Longitude", "Latitude"), crs = st_crs(4326)) #the original crs is 4326 !!
rescoring_iris <- st_transform(rescoring_iris, crs =  2154)

# Match IRIS data with UWS data ----

rescoring_iris <- st_join(rescoring_iris,
                          IRIS_DATA,
                          left = TRUE)

# clean environment ----
# Compare old and new scores ----

rm(nin, sev)

# Add recored values to original data
rescored_UWS <- uws_paris_iris %>%
  left_join( rescoring, suffix = c("", "_rescored"), 
             by = c("ID_site" = "ID_site") )


