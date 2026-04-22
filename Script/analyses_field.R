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

# regression lineaire google vs field

ggplot(data = transect_indices,
       mapping = aes(x = `Total wildness_google`, y = `Total wildness_field`)) +
  geom_point() +
  geom_abline(slope = 1, intercept =0 ) + 
  ylim (0,5) + xlim (0,5)

# test de correlation de spearman (correlation des rangs)
cor.test( transect_indices$`Total wildness_google`, 
          transect_indices$`Total wildness_field`,
          method = 'spearman')

#Composition and UWs & richness ----


