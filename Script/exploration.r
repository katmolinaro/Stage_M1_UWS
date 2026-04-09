# Explore data for PAris UWS
library(DHARMa)
library(lme4)
library(lmerTest)

#EXCLUDE THE 0 UWS BECAUSE IT INFLUENCES OUR MEAN/MEDIAN ----

uws_iris_clean <- uws_paris_iris %>%
  filter(
    `Total wildness` != 0,   # keep total wildness different from 0
    !is.na(X_revenus_m)          # keep if income different from NA
  )

# Check the number of lines 
cat("Number of lines before the filter :", nrow(uws_paris_iris), "\n")
cat("Number of lines after the filter :", nrow(uws_iris_clean), "\n")

# Vizualise distribtuion of scores ----

boxplot(`Total wildness`  ~ Arr , 
        data = uws_iris_clean )
boxplot(`X_revenus_m`  ~ Arr , 
        data = uws_iris_clean )

# Graph sur tout paris
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

# comparaison
f1a = lm(`Total wildness`  ~ Arr , 
          data = uws_iris_clean )

f1b = lm(`Total wildness`  ~ `X_revenus_m` , 
     data = uws_iris_clean )
summary(f1b)

f2 = lm(`Total wildness`  ~ `X_revenus_m` + Arr  , 
        data = uws_iris_clean )
summary(f2)
anova(f2)

f3 = lm(`Total wildness` ~  `X_revenus_m`*Arr , 
        data = uws_iris_clean )
summary(f3)
anova(f3)
plot(f3)
hist(residuals(f3))


anova(f1b,f3)

# test modèle linéaire mixte
f0 = lme4::lmer(`Total wildness`  ~ 1 + (1|Arr) , 
                 data = uws_iris_clean )

f1 = lme4::lmer(`Total wildness`  ~ `X_revenus_m` + (1|Arr) , 
          data = uws_iris_clean )
summary(f1a) 

f1 = lme4::lmer(`Total wildness`  ~ `X_revenus_m` + (1|Arr) , 
                data = uws_iris_clean )
summary(f1a) 

anova(f0, f1)

plot(DHARMa::simulateResiduals(f1))
plot(f1)
