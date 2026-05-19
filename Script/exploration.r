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
# cat("Number of lines before the filter :", nrow(uws_paris_iris), "\n")
# cat("Number of lines after the filter :", nrow(uws_iris_clean), "\n")

# Vizualise distribtuion of scores ----

boxplot(`Total wildness`  ~ Arr , 
        data = uws_iris_clean )

ggplot(uws_iris_clean, aes(x=Arr, y=`Total wildness`, fill=Arr)) +
  geom_boxplot()

boxplot(`X_revenus_m`  ~ Arr , 
        data = uws_iris_clean )

ggplot(uws_iris_clean, aes(x=Arr, y=`X_revenus_m`, fill=Arr)) +
  geom_boxplot()

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
summary(f1a)

f1b = lm(`Total wildness`  ~ `X_revenus_m` , 
     data = uws_iris_clean )
summary(f1b)

f2 = lm(`Total wildness`  ~ `X_revenus_m` + Arr  , 
        data = uws_iris_clean )
summary(f2)
anova(f2)

f2b = lm(`Total wildness`  ~ Arr +  `X_revenus_m` , 
        data = uws_iris_clean )
summary(f2b)

f3b = lm(`Total wildness` ~  Arr *  `X_revenus_m` , 
        data = uws_iris_clean )
summary(f3b)

f3 = lm(`Total wildness` ~  `X_revenus_m`*Arr , 
        data = uws_iris_clean )
summary(f3)
anova(f3)

plot(f3)
hist(residuals(f3))

anova(f1a,f2b,f3b)
anova(f1b,f2,f3)

# test modèle linéaire mixte
f0m = lme4::lmer(`Total wildness`  ~ 1 + (1|Arr) , 
                 data = uws_iris_clean )
# summary(f0)


f1m = lme4::lmer(`Total wildness`  ~ `X_revenus_m` + (1|Arr) , 
          data = uws_iris_clean )

summary(f1)
# 
# f2m = lme4::lmer(`Total wildness`  ~ `X_revenus_m` * (1|Arr) , 
#                 data = uws_iris_clean )
# summary(f1a) 

anova(f0m, f1m)

plot(DHARMa::simulateResiduals(f1m))
plot(f1m)


# Effet de la vegetalisation ----

##filter to only have 1 and 2 in types ----

uws_vegeza <- filter(.data = uws_iris_clean,
                    Type != c("0"))

## graph ----

ggplot2::ggplot(data = uws_vegeza,
                mapping = aes(x = as.factor(Type),
                              y = `Total wildness`,
                              colour = Arr
                )
) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap('Arr')


ggplot(data = uws_vegeza,
       mapping = aes(x = as.factor(Type),
                     y = `Total wildness`,
                     color = Type ))+
  geom_boxplot()

## test

var.test(uws_vegeza$`Total wildness`~ uws_vegeza$Type)
# p-value = 0.5867

t.test(uws_vegeza$`Total wildness`~ uws_vegeza$Type, var.equal = T)


# # DATE ANALYSE ----
# 
# ## transform ----
# 
# uws_date <- filter(uws_iris_clean, Date != is.na(uws_iris_clean$Date))
# 
# uws_date <- separate (uws_date, col = Date, into = c("month", "year"), sep = " ")
# 
# 
# ## graph ----
# 
# ggplot(data = uws_date,
#        mapping = aes(x = as.factor(year),
#                      y = `Total wildness`,
#                      color = year ))+
#   geom_boxplot()
# 








