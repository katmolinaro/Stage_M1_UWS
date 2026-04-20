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
