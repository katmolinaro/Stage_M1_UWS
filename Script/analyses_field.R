# Analyses

library(DHARMa)
library(lme4)
library(lmerTest)
library(ggplot2)
library(glmmTMB)

# Richness and UWS field ----

## Graph ----
### Graph sur tout paris ----

# ggplot2::ggplot(data = transect_indices,
#                 mapping = aes(x = richness,
#                               y = `Total wildness_field`
#                 )
# ) +
#   geom_point() +
#   geom_smooth(method = "lm")

#et richness en fonction UWS

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

#### density ----
ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = sp_density
                )
) +
  geom_point() +
  geom_smooth(method = "lm")


#### wild (non-horticultural) ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness_wild
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

### Graph par quartier ----

# ggplot2::ggplot(data = transect_indices,
#                 mapping = aes(x = richness,
#                               y = `Total wildness_field`,
#                               colour = Arr_field
#                 )
# ) +
#   geom_point() +
#   geom_smooth(method = "lm")

#et richness en fonction uws
ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness,
                              colour = Arr_field
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

####density ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = sp_density,
                              colour = Arr_field
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

#SACHANT que le point dans le 5e sera une valeur abhérante lorsque j'ajouterai les 40 plantes horticoles...

#### wild (non-horticultural) ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness_wild,
                              colour = Arr_field
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

## Test modèle linéaire simple ----

f1 = lm(`richness`  ~ `Total wildness_field` , 
              data = transect_indices  )
summary(f1)
plot(DHARMa::simulateResiduals(f1))


f1_pois = glm(`richness`  ~ `Total wildness_field` , 
         data = transect_indices, 
         family = poisson  )
summary(f1a)
plot(DHARMa::simulateResiduals(f1a))
# pas terrible avec poisson



####density ----

f1 = lm(sp_density  ~ `Total wildness_field` , 
        data = transect_indices  )
summary(f1)
plot(DHARMa::simulateResiduals(f1))
hist(f1$residuals)


# f1_pois = glm(sp_density  ~ `Total wildness_field` , 
#               data = transect_indices, 
#               family = poisson  )
# summary(f1_pois)
# plot(DHARMa::simulateResiduals(f1_pois))

#### wild (non-horticultural) ----

f1 = lm(richness_wild  ~ `Total wildness_field` , 
        data = transect_indices  )
summary(f1)
plot(DHARMa::simulateResiduals(f1))

#p-value = 0.0001883 !!!!!!!!!

f1_pois = glm(richness_wild  ~ `Total wildness_field` , 
              data = transect_indices, 
              family = poisson  )
summary(f1_pois)
plot(DHARMa::simulateResiduals(f1_pois))




# Richness and income ----

## Graph ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = X_revenus_m,
                              y = richness
                )
) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = X_revenus_m,
                              y = richness,
                              colour = Arr_field
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

#En fait la tendance générale est neutre parcequ'il y a deux tendances parfaitement opposées dans chaque arrondissement


## Test ----
### Test linéaire simple ----
f_arr = lm(richness  ~ Arr_field ,
          data = transect_indices )
summary(f1a)

### tester effet revenus

f1 = lm(`richness`  ~ `Total wildness_field` , 
        data = filter(transect_indices, !is.na(X_revenus_m) ) )

f1b = lm(richness  ~ `X_revenus_m` ,
         data = transect_indices )
summary(f1b)

f2 = lm(richness  ~ `Total wildness_field` + X_revenus_m   ,
        data = transect_indices )
summary(f2)
anova(f2)


f3 = lm(richness ~  `Total wildness_field`*`X_revenus_m` ,
        data = transect_indices )
summary(f3)
anova(f3)

f3b = lm(richness ~  `Total wildness_field`*Arr_field ,
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3b)
anova(f3b)

# comparaison de modèles
anova(f1,f2,f3) 
anova(f1,f3b)

# test de l'interaction possible entre revenus et arrondissement sur richesse

f4 = lm(richness ~ `X_revenus_m` * Arr_field ,
        data = transect_indices )
summary(f4)
anova(f4)
anova(f1b,f4)
 # effet marginal


# Richness formal and informal ----





#Composition and UWs & richness ----


