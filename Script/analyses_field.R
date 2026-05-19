# Analyses

library(DHARMa)
library(lme4)
library(lmerTest)
library(ggplot2)
library(glmmTMB)


# General richness of sidewalks ----


## Richesse par arrondissement communes ----

# mean(transect_indices$richness_wild)
# sd(transect_indices$richness_wild)

nin <- filter(transect_indices, Arr_field == "19e")
sev <- filter(transect_indices, Arr_field == "7e")

mean(nin$richness_wild)
sd(nin$richness_wild)
mean(sev$richness_wild)
sd(sev$richness_wild)


ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Arr_field),
                              y = richness_wild
                )
) +
  geom_boxplot()

f = lm(richness_wild  ~ Arr_field ,
            data = transect_indices )
summary(f)




mean(nin$richness)
sd(nin$richness)
mean(sev$richness)
sd(sev$richness)

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Arr_field),
                              y = richness
                )
) +
  geom_boxplot()

f = lm(richness  ~ Arr_field ,
       data = transect_indices )
summary(f)




## Courbe aire-espèces  ####

## Tendance générale 
ggplot2::ggplot(data = transect_indices) +
  geom_point(mapping = aes(x = area,
                           y = richness,
                           colour = as.factor(Type_field)),
             shape = 1) +
  geom_smooth(mapping = aes(x = area,
                            y = richness),
              method = "gam", colour = "black") +
  theme_bw()


ggplot2::ggplot(data = transect_indices) +
  geom_point(mapping = aes(x =area,
                           y = richness_wild,
                           colour = as.factor(Type_field))) +
  geom_smooth(mapping = aes(x = area,
                            y = richness_wild),
              method = "gam",
              linetype = "solid", colour = "black") +
  theme_bw()


## Relationships within vegetation types ----

#richesse wild en fonction vegetalisation --> significatif
ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Type_field),
                              y = richness_wild
                )
) +
  geom_boxplot()

noveg <- filter(transect_indices, Type_field == 1)
veg <- filter (transect_indices, Type_field == 2)

mean(noveg$richness_wild)
sd(noveg$richness_wild)
mean(veg$richness_wild)
sd(veg$richness_wild)

t.test(transect_indices$richness_wild~transect_indices$Type_field)


# Surface en fonction vegetalsation --> Significatif 

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Type_field),
                              y = area
                )
) +
  geom_boxplot()

t.test(transect_indices$area~transect_indices$Type_field)


#Richesse en fonciton des habitats formel ??
f = lm(richness_wild ~ as.factor(formal),
       data = transect_indices )
summary(f)
anova(f)

#richesse en fonciton de l'area

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = area,
                              y = richness_wild,
                              colour = as.factor(Type_field))) +
  geom_point() +
  geom_smooth( method = "lm") +
  theme_bw()



# Effet du nombre d'habitats
ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(informal),
                              y = richness_wild)) +
  geom_boxplot() +
  theme_bw()


  
  

  
# Richness and income ----
  
# Wild richness vs income
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = X_revenus_m,
                                y = richness_wild
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")

  f = lm(richness_wild  ~ `X_revenus_m` ,
         data = transect_indices )
  summary(f)
  
  # Tendance par arrondissement: 
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = X_revenus_m,
                                y = richness_wild,
                                colour = Arr_field
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  #En fait la tendance générale est neutre parcequ'il y a deux tendances parfaitement opposées dans chaque arrondissement
  
  f = lm(richness_wild  ~ X_revenus_m * Arr_field ,
         data = transect_indices )
  summary(f)

  f = lm(richness_wild  ~ X_revenus_m ,
         data = filter(transect_indices, Arr_field == "19e" ))
  summary(f)
  f = lm(richness_wild  ~ X_revenus_m ,
         data = filter(transect_indices, Arr_field == "7e" ))
  summary(f)
  
  
  # Tendance par type de trottoir
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = X_revenus_m,
                                y = richness_wild,
                                colour = as.factor(Type_field)
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  f = lm(richness_wild  ~ X_revenus_m * as.factor(Type_field) ,
         data = transect_indices )
  summary(f)
  
  f = lm(richness_wild  ~ X_revenus_m ,
         data = filter(transect_indices, Type_field ==1 ))
  summary(f)
  f = lm(richness_wild  ~ X_revenus_m ,
         data = filter(transect_indices, Type_field ==2 ))
  summary(f)
   
  
# Richness and UWS field ----

## Richness wild as a funciton of UWS scores ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness_wild
                )
) +
  geom_point() +
  geom_smooth(method = "lm") +
    theme_bw()
  
## Test modèles différents ----
  
  # Linear model # the choice!
  f1 = lm(richness_wild  ~ `Total wildness_field` , 
          data = transect_indices  )
  summary(f1)
  plot(DHARMa::simulateResiduals(f1))
  
  # poisson distribution
  f1_pois = glm(`richness_wild`  ~ `Total wildness_field` , 
                data = transect_indices, 
                family = poisson  )
  summary(f1_pois)
  plot(DHARMa::simulateResiduals(f1_pois))
  # pas terrible avec poisson
  
  # Negative binomial distribution
  f1_nbinom = glmmTMB(`richness_wild`  ~ `Total wildness_field` , 
                data = transect_indices, 
                family = nbinom1  )
  summary(f1_nbinom)
  plot(DHARMa::simulateResiduals(f1_nbinom))
  

  ### Effet des types de vegetation : ----
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = `Total wildness_field`,
                                y = richness_wild,
                                colour = as.factor(Type_field)
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  f = lm(richness_wild  ~  as.factor(Type_field)*`Total wildness_field` , 
          data = transect_indices  )
  summary(f)
  anova(f)
  # les deux variables ont un effet significatif mais l'interaction n'est pas significative (= pas de différence de pente)
  
  ### Effet de l'arrondissement : ----
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = `Total wildness_field`,
                                y = richness_wild,
                                colour = as.factor(Arr_field)
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  f = lm(richness_wild  ~  as.factor(Arr_field)*`Total wildness_field` , 
         data = transect_indices  )
  summary(f)
  anova(f)
  # la tendance reste stable (meme pente) entre les deux arrondissements.
  
#### density ----
ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = sp_density
                )
) +
  geom_point() +
  geom_smooth(method = "lm")

  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = `Total wildness_field`,
                                y = sp_density
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  ggplot2::ggplot(data = transect_indices,
                  mapping = aes(x = `Total wildness_field`,
                                y = sp_density
                  )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
  # densité avec interaction arrondissement
  f = lm(sp_density  ~ `Total wildness_field` , 
          data = transect_indices  )
  summary(f)
  plot(DHARMa::simulateResiduals(f))
  
  f = lm(sp_density ~  as.factor(Arr_field)*`Total wildness_field` , 
         data = transect_indices  )
  summary(f)
  anova(f)
  
  # densité avec interaction arrondissement
  f = lm(sp_density  ~ as.factor(Type_field)*`Total wildness_field` , 
          data = transect_indices  )
  summary(f)
  anova(f)
  
  # Pas d'effet d'interaction avec Arrondissement ou bien type de trottoir
  
####with horticultural plants ----

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = `Total wildness_field`,
                              y = richness
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
hist(f1$residuals)
plot(DHARMa::simulateResiduals(f1))

#p-value = 0.0001883 !!!!!!!!!

f1_pois = glm(richness_wild  ~ `Total wildness_field` , 
              data = transect_indices, 
              family = poisson  )
summary(f1_pois)
plot(DHARMa::simulateResiduals(f1_pois))



## Test richesse----
### Test linéaire simple ----
f1_arr = lm(richness  ~ Arr_field ,
          data = transect_indices )
summary(f1_arr)

f1_inc = lm(richness  ~ `X_revenus_m` ,
            data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f1_inc)

f1_wil = lm(`richness`  ~ `Total wildness_field` , 
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f1_wil)

### ajoute les effets revenus ----

f2 = lm(richness  ~ `Total wildness_field` + X_revenus_m   ,
        data = filter(transect_indices, !is.na(X_revenus_m)) )
summary(f2)
anova(f2)


f3 = lm(richness ~  `Total wildness_field`*`X_revenus_m` ,
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3)
anova(f3)

f3b = lm(richness ~  `Total wildness_field`*Arr_field ,
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3b)
anova(f3b)

# comparaison de modèles
anova(f1_wil,f2,f3) 
anova(f1_wil,f3b)

# test de l'interaction possible entre revenus et arrondissement sur richesse

f4 = lm(richness ~ `X_revenus_m` * Arr_field ,
        data = transect_indices )
summary(f4)
anova(f4)
anova(f1_inc,f4)
 # effet marginal



## Test density----
### Test linéaire simple ----
f1_arr = lm(sp_density  ~ Arr_field ,
            data = transect_indices )
summary(f1_arr)

f1_inc = lm(sp_density  ~ `X_revenus_m` ,
            data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f1_inc)

f1_wil = lm(`sp_density`  ~ `Total wildness_field` , 
            data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f1_wil)

### ajoute les effets revenus ----

f2 = lm(sp_density  ~ `Total wildness_field` + X_revenus_m   ,
        data = filter(transect_indices, !is.na(X_revenus_m)) )
summary(f2)
anova(f2)


f3 = lm(sp_density ~  `Total wildness_field`*`X_revenus_m` ,
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3)
anova(f3)

f3b = lm(sp_density ~  `Total wildness_field`*Arr_field ,
         data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3b)
anova(f3b)

# comparaison de modèles
anova(f1_wil,f2,f3) 
anova(f1_wil,f3b)

# test de l'interaction possible entre revenus et arrondissement sur richesse

f4 = lm(sp_density ~ `X_revenus_m` * Arr_field ,
        data = transect_indices )
summary(f4)
anova(f4)
anova(f1_inc,f4)



## Test richesse WILD----
f1_wil = lm(`richness_wild`  ~ `Total wildness_field` , 
            data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f1_wil)

### ajoute les effets revenus ----

f2 = lm(richness_wild  ~ `Total wildness_field` + X_revenus_m   ,
        data = filter(transect_indices, !is.na(X_revenus_m)) )
summary(f2)
anova(f2)


f3 = lm(richness_wild ~  `Total wildness_field`*`X_revenus_m` ,
        data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3)
anova(f3)

f3b = lm(richness_wild ~  `Total wildness_field`*Arr_field ,
         data = filter(transect_indices, !is.na(X_revenus_m) ) )
summary(f3b)
anova(f3b)

# comparaison de modèles
anova(f1_wil,f2,f3) 
anova(f1_wil,f3b)

# test de l'interaction possible entre revenus et arrondissement sur richesse

f4 = lm(richness_wild ~ `X_revenus_m` * Arr_field ,
        data = transect_indices )
summary(f4)
anova(f4)
anova(f1_inc,f4)
















ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Type_field),
                              y = richness_informal
                )
) +
  geom_boxplot()

ggplot2::ggplot(data = transect_indices,
                mapping = aes(x = as.factor(Type_field),
                              y = `Total wildness_field`
                )
) +
  geom_boxplot()


#Composition and UWs & richness ----


