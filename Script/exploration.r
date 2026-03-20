# Explore data for PAris UWS

# Vizualise distribtuion of scores

boxplot(`Total wildness`  ~ Place , 
        data = UWS_iris )
boxplot(`_revenus_m`  ~ Place , 
        data = UWS_iris )

# Graph sur tout paris
plot(`Total wildness`  ~ `_revenus_m` , 
     data = UWS_iris )

ggplot2::ggplot(data = UWS_iris,
                mapping = aes(x = `_revenus_m`,
                              y = `Total wildness`
                              )
                ) +
  geom_point() +
  geom_smooth(method = "lm")

# graph par quartier
ggplot2::ggplot(data = UWS_iris,
                mapping = aes(x = `_revenus_m`,
                              y = `Total wildness`,
                              colour = Place
                )
) +
  geom_point() +
  geom_smooth(method = "lm")


# test modèle linéaire simple


f1a = aov(`Total wildness`  ~ Place , 
        data = UWS_iris )
summary(f1a)
TukeyHSD(f1a)

f1b = lm(`Total wildness`  ~ `_revenus_m` , 
     data = UWS_iris )
summary(f1b)

f2 = lm(`Total wildness`  ~ Place + `_revenus_m` , 
        data = UWS_iris )
summary(f2)

f3 = lm(`Total wildness`  ~ Place * `_revenus_m`, 
        data = UWS_iris )
summary(f3)

anova( f1b,f2,f3)
