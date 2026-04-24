#  Import vegetation ----

veg_survey = read_excel(path = "Raw/SDMR_FIELD.xlsx",
                        sheet = "espèces")

## Replace NA by 0 in the surveys : ----
veg_survey[, 4:ncol(veg_survey)] [is.na(veg_survey[, 4:ncol(veg_survey)])] <- 0

## Extract unique species names ---- 
species <- sort(unique(veg_survey$Especes))


# Import metadata trottoir ----

veg_transect = read_excel(path = "Raw/SDMR_FIELD.xlsx",
                        sheet = "transect",
                        skip = 1)


## Replace NA by 0 in the type of microhabitat --> 0 means does not exist ----
veg_transect[, 13:ncol(veg_transect)] [is.na(veg_transect[, 13:ncol(veg_transect)])] <- 0

names(veg_transect) <- str_replace_all(names(veg_transect),pattern = " ", replacement = "_")

#Rename Buf to 5E_Buf

veg_survey[veg_survey$Code_trottoir == "Buf","Code_trottoir"] <- "5E_Buf"
veg_transect$Code_trottoir <- str_replace(string = veg_transect$Code_trottoir, pattern = "Buf", replacement = "5E_Buf")

# Create habitat groups

veg_survey <- veg_survey %>%
  mutate(informal = (Caniveau + Fissure_bas_de_mur +Murs + Joints_de_transition + Chemin + Autres) > 0,
         formal = (Pied_arbre_isole + Pelouse_arbre + Pelouse_sans + Massif_arbre + Massif_sans) > 0 )

veg_transect <- veg_transect %>%
  mutate(informal = (Caniveau + Fissure_de_bas_de_mur +Murs + Joints_de_transition + Chemins + Autres),
         formal = (`Pied_d'arbre_isolé` + Pelouse_arbre + Pelouse_Sans + Massif_arbre + Massif_sans) )


hist(veg_transect$formal)

