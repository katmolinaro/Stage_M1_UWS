# calculate indices for each transect and summarise them in one table ----

# Total richness per transect ----

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  summarise(richness = n()) # 'n()' compte le nombre de lignes par groupe

# Left join different indices to transect dataframe ----

transect_indices <- veg_transect %>%
  left_join(richness, by = "Code_trottoir") %>%
  left_join(y = uws_field, by = c("Code_trottoir" = "ID_site") ) %>%
  left_join( UWS_Paris, suffix = c("_field", "_google"), 
            by = c("Code_trottoir" = "ID_site") )

# Spatial object ----

transect_indices <- st_as_sf(transect_indices, coords = c("Longitude_field", "Latitude_field"), crs = st_crs(4326)) #the original crs is 4326 !!
transect_indices <- st_transform(transect_indices, crs =  2154)


#Match the IRIS----

transect_indices <- st_join(transect_indices,
           IRIS_DATA,
           left = TRUE)

#Clean environment ----
rm(richness)

