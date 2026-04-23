# calculate indices for each transect and summarise them in one table ----

# Total richness per transect ----

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  summarise(richness = n()) # 'n()' compte le nombre de lignes par groupe

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  filter_out(Horticole == "1") %>%
  summarise(richness_wild = n()) %>% 
  left_join(richness, by = "Code_trottoir")
  
#plot(richness~richness_wild, data = richness)

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  filter_out(informal == FALSE) %>%
  summarise(richness_informal = n()) %>% 
  right_join(richness, by = "Code_trottoir")

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  filter_out(formal == FALSE) %>%
  summarise(richness_formal = n()) %>% 
  right_join(richness, by = "Code_trottoir")

# Left join different indices to transect dataframe ----

transect_indices <- veg_transect %>%
  left_join(richness, by = "Code_trottoir") %>%
  left_join(y = uws_field, by = c("Code_trottoir" = "ID_site") ) %>%
  left_join( UWS_Paris, suffix = c("_field", "_google"), 
            by = c("Code_trottoir" = "ID_site") ) %>%
  left_join( rescoring, suffix = c("", "_rescored"), 
             by = c("Code_trottoir" = "ID_site") )


# Spatial object ----

transect_indices <- st_as_sf(transect_indices, coords = c("Longitude_field", "Latitude_field"), crs = st_crs(4326)) #the original crs is 4326 !!
transect_indices <- st_transform(transect_indices, crs =  2154)


#Match the IRIS----

transect_indices <- st_join(transect_indices,
           IRIS_DATA,
           left = TRUE)

# Calculate the surface per transect & species density ----

transect_indices <- transect_indices %>%
  mutate(area = (Maximale + Minimale)*20/2,
         sp_density = richness/area)


#Clean environment ----
rm(richness)

#exploration rapide

# plot(transect_indices$`Total wildness_google` ~transect_indices$`Total wildness`)
# abline(a=0, b=1)
# identify(transect_indices$`Total wildness_google` ~transect_indices$`Total wildness`)
#lignes  2  5  6  9 11