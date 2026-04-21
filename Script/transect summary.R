# calculate indices for each transect and summarise them in one table ----

# Total richness per transect ----

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  summarise(richness = n()) # 'n()' compte le nombre de lignes par groupe

# Left join different indices to transect dataframe ----

transect_indices <- veg_transect %>%
  left_join(richness, by = "Code_trottoir") %>%
  left_join(y = uws_field, by = c("Code_trottoir" = "ID_site") )


#Clean environment ----
rm(richness)

