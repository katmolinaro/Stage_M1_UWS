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

# Compare old and new scores ----


# Add recored values to original data
rescored_UWS <- uws_paris_iris %>%
  left_join( rescoring, suffix = c("", "_rescored"), 
             by = c("ID_site" = "ID_site") )



## Regression lineaire ----
# mean absolute difference: 
mean(abs(rescored_UWS$`Total wildness_rescored`- rescored_UWS$`Total wildness`), na.rm = TRUE)


## Paired t-test ----
# mean absolute difference: 
mean(rescored_UWS$`Total wildness_rescored`- rescored_UWS$`Total wildness`, na.rm = TRUE)
# on average scores are 0,094 higher in rescored points

t.test(rescored_UWS$`Total wildness_rescored`, rescored_UWS$`Total wildness`,
       alternative = "less",
       paired = TRUE)
# No significant trend in the difference = no systematic under or over scoring. 

## test de correlation de spearman (correlation des rangs) ----
cor.test(rescored_UWS$`Total wildness`, 
         rescored_UWS$`Total wildness_rescored`,
         method = 'spearman')
# Significant

## Linear regression (RMS) ----
summary( rescored_reg <- lm(`Total wildness_rescored`~`Total wildness`, data = rescored_UWS))
plot(simulateResiduals(rescored_reg)) # not great

# mean error:
mean(abs(residuals(rescored_reg)))


# R2 = 0.49 ; Equation : UWS_rescored = 0,7284 + 0,6922 * UWS_old
# Regression not horrible but not very tight.
# Need to check against rescored katia data


## SMA (Single major axis estimation) ----
library(smatr)

(rescored_sma <- sma(`Total wildness_rescored`~ `Total wildness`,
                     data = rescored_UWS,
                     slope.test = 1,
                     elev.test = 0
                     )
 )
# Slope is not different from 1 and intercept not different from 0. Victory !

plot(rescored_sma)
plot(rescored_sma , which = "residual")

# A few visible oultiers:
plot(`Total wildness_rescored`~ `Total wildness`,  data = rescored_UWS)
# outliers <-  identify(rescored_UWS$`Total wildness`,rescored_UWS$`Total wildness_rescored`,
                    labels = rescored_UWS$ID_site )
# 88  92  94 106 112
rescored_UWS$ID_site[outliers]
#  "19E_2"  "19E_6"  "19E_8"  "19E_20" "19E_26"


# Graph ----
## Simple linear regression graph ----
ggplot(data = rescored_UWS,
       mapping = aes(x = `Total wildness`, y = `Total wildness_rescored`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "grey",
              linetype = "solid", linetype = "dotted") +
  geom_abline(aes(slope =  rescored_reg$coefficients[2],
                  intercept =  rescored_reg$coefficients[1]),  col = "darkblue") +
  ylim (0,5) + xlim (0,5) +
  geom_text(mapping = aes(x = x, y = y, label = label),
            data = data_frame(x = c(2.5, 2.5), 
                              y = c(1, 0.7), 
                              label = c("R2 = 0.65",
                                        "UWS_rescored =  -0,2703 + 1,0633 * UWS_katia")),
            hjust = "left", size =3) +
  theme_minimal()

## Model comparison graph ----
ggplot(data = rescored_UWS,
       mapping = aes(x = `Total wildness`, y = `Total wildness_rescored`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "grey",linetype = "solid") +
  geom_abline(aes(slope =  rescored_reg$coefficients[2],
                  intercept =  rescored_reg$coefficients[1],
                  col = "lm",linetype = "free")) +
  geom_abline(aes(slope = rescored_sma$groupsummary$Slope,
                  intercept = rescored_sma$groupsummary$Int,
                  col = "sma",linetype = "free")) +
    scale_linetype_manual(name = "Intercept", values = c("free" = "solid", "via0" = "dashed")) + 
  scale_color_manual(name = "Models", values = c("lm" = "darkblue", "sma" = "red")) + 
  ylim (0,5) + xlim (0,5) +
  theme_minimal()
