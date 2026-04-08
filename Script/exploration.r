# Explore data for PAris UWS

#EXCLUDE THE 0 UWS BECAUSE IT INFLUENCES OUR MEAN/MEDIAN ----

uws_iris_clean <- uws_paris_iris %>%
  filter(
    `Total wildness` != 0,   # keep total wildness different from 0
    !is.na(X_revenus_m)          # keep if income different from NA
  )

# Check the number of lines 
cat("Number of lines before the filter :", nrow(uws_paris_iris), "\n")
cat("Number of lines after the filter :", nrow(uws_iris_clean), "\n")

#TEST - selection of field points along a wildness gradient, inside a district, with similar income ----

field <- uws_iris_clean %>%
  filter(
    Arr == "Ivry",   # keep if we have irvy
    X_revenus_m > 17000 & X_revenus_m < 22000         # keep if 17000<income<22000
  )

nrow(field)

# Vizualise distribtuion of scores ----

boxplot(`Total wildness`  ~ Arr , 
        data = uws_iris_clean )
boxplot(`X_revenus_m`  ~ Arr , 
        data = uws_iris_clean )

# Graph sur tout paris
plot(`Total wildness`  ~ `X_revenus_m` , 
     data = uws_iris_clean )

ggplot2::ggplot(data = uws_iris_clean,
                mapping = aes(x = `X_revenus_m`,
                              y = `Total wildness`
                              )
                ) +
  geom_point() +
  geom_smooth(method = "lm")

# graph par quartier
ggplot2::ggplot(data = uws_iris_clean,
                mapping = aes(x = `X_revenus_m`,
                              y = `Total wildness`,
                              colour = Arr
                )
) +
  geom_point() +
  geom_smooth(method = "lm")


# test modèle linéaire simple


f1a = aov(`Total wildness`  ~ Arr , 
        data = uws_iris_clean )
summary(f1a) 
TukeyHSD(f1a)

f1b = lm(`Total wildness`  ~ `X_revenus_m` , 
     data = uws_iris_clean )
summary(f1b)

f2 = lm(`Total wildness`  ~ Arr + `X_revenus_m` , 
        data = uws_iris_clean )
summary(f2)

f3 = lm(`Total wildness`  ~ Arr * `X_revenus_m`, 
        data = uws_iris_clean )
summary(f3)

anova( f1b,f2,f3)
