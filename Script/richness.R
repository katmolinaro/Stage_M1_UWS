# calculate richness ----

# Total richness per transect ----

richness <- veg_survey %>%
  group_by(Code_trottoir) %>%
  summarise(richness = n()) # 'n()' compte le nombre de lignes par groupe

# Add richness to transect dataframe ----

richness_transect <- veg_transect %>%
  left_join(richness, by = "Code_trottoir")

#Clean environment ----
rm(richness)

