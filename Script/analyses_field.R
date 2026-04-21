# Analyses

library(DHARMa)
library(lme4)
library(lmerTest)
library(ggplot2)

#Richness and UWS field ----

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

#SACHANT que le point dans le 5e sera une valeur abhérante lorsque j'ajouterai les 40 plantes horticoles...


## Test modèle linéaire simple ----

f1a = lm(`richness`  ~ `Total wildness_field` , 
         data = transect_indices )
summary(f1a)
plot(f1a)

f1b = lm(`Total wildness_field`  ~ `richness` , 
         data = transect_indices )
summary(f1b)
plot(f1b)

#les résidus sont...... bon


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
f1a = aov(richness  ~ Arr_field ,
          data = transect_indices )
summary(f1a)
TukeyHSD(f1a)


### comparaison ----
f1a = lm(richness  ~ Arr_field ,
         data = transect_indices )
summary(f1a)

f1b = lm(richness  ~ `X_revenus_m` ,
         data = transect_indices )
summary(f1b)

f2 = lm(richness  ~ `X_revenus_m` + Arr_field  ,
        data = transect_indices )
summary(f2)
anova(f2)


f3 = lm(richness ~  `X_revenus_m`*Arr_field ,
        data = transect_indices )
summary(f3)
anova(f3)

plot(f3)
hist(residuals(f3))


anova(f1b,f2,f3)
 
### test modèle linéaire mixte ----
f0 = lme4::lmer(richness  ~ 1 + (1|Arr_field) ,
                data = transect_indices )

f1 = lme4::lmer(richness  ~ `X_revenus_m` + (1|Arr_field) ,
                data = transect_indices )

summary(f1)

f1 = lme4::lmer(richness  ~ `X_revenus_m` + (1|Arr_field) ,
                data = transect_indices )
summary(f1a)

anova(f0, f1)


plot(DHARMa::simulateResiduals(f1))
plot(f1)




#UWS field and UWS google ----

## Transform the table ----

#maybe not the most smart to do but my brain is fixed on this method

uws_comparaison <- select(.data = transect_indices, Code_trottoir,`Total wildness_field`,`Total wildness_google` )

uws_comparaison <- pivot_longer(uws_comparaison, cols = c("Total wildness_field", "Total wildness_google"), names_to = "Traitement", values_to = "Total_wildness")

## Graph ----


ggplot(uws_comparaison, aes(x = Traitement, y = Total_wildness, group = Code_trottoir, color = factor(Code_trottoir))) +
  geom_line() +
  geom_point() +
  labs(y = "Total wildness",
       x = "Traitement",
       subtitle = "Wildness en fonction du traitement soit field ou google",
       color = "Code trottoir")


## Test ----

#test non-paramétrique pour données appariées de wilcoxon

wilcox.test(transect_indices$`Total wildness_field`, transect_indices$`Total wildness_google`, paired=TRUE)
#p-value = 0.006925 << 0.05, significatif, changement entre field et google

summary(transect_indices)
#Total wildness_field
#Median :2.500
#Mean   :2.428

#Total wildness_google
#Median :1.8056
#Mean   :1.9596



#Composition and UWs & richness ----


