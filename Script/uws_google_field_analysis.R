# UWS google and UWS field analysis ----

# Transform the table ----

#maybe not the most smart thing to do but my brain is fixed on this method

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


### Test ----

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

## Regression lineaire google vs field ----

ggplot(data = transect_indices,
       mapping = aes(x = `Total wildness_google`, y = `Total wildness_field`)) +
  geom_point() +
  geom_abline(slope = 1, intercept =0 ) + 
  ylim (0,5) + xlim (0,5)

### test de correlation de spearman (correlation des rangs) ----
cor.test( transect_indices$`Total wildness_google`, 
          transect_indices$`Total wildness_field`,
          method = 'spearman')


#RESCORED ----

## Regression lineaire rescored vs field ----

ggplot(data = transect_indices,
       mapping = aes(x = `Total wildness`, y = `Total wildness_field`)) +
  geom_point() +
  geom_abline(slope = 1, intercept =0 ) + 
  ylim (0,5) + xlim (0,5)

## test de correlation de spearman (correlation des rangs) ----
cor.test( transect_indices$`Total wildness`, 
          transect_indices$`Total wildness_field`,
          method = 'spearman')


## Rescored vs google ----

ggplot(data = transect_indices,
       mapping = aes(x = `Total wildness`, y = `Total wildness_google`)) +
  geom_point() +
  geom_abline(slope = 1, intercept =0 ) + 
  ylim (0,5) + xlim (0,5)

#test de correlation

cor.test( transect_indices$`Total wildness`, 
          transect_indices$`Total wildness_google`,
          method = 'spearman')



# DATE ANALYSE ----

## transform ----

# uws_date <- filter(transect_indices, Date != is.na(transect_indices$Date_rescored))

uws_date <- transect_indices

uws_date <- separate (uws_date, col = Date_rescored, into = c("month", "year"), sep = " ")


## graph ----

# ggplot(data = uws_date,
#        mapping = aes(x = as.factor(year),
#                      y = `Total wildness`,
#                      color = year ))+
#   geom_boxplot()

ggplot(data = uws_date,
       mapping = aes(x = `Total wildness`, y = `Total wildness_field`, colour = year)) +
  geom_point() +
  geom_abline(slope = 1, intercept =0 ) + 
  ylim (0,5) + xlim (0,5)

## check autre graph

uws_date_v2 <- pivot_longer(uws_date, cols = c("Total wildness_field", "Total wildness"), names_to = "Traitement", values_to = "Total_wildness")

ggplot(data = uws_date_v2,
       mapping = aes(x = as.factor(year),
                     y = `Total_wildness`,
                     color = Traitement ))+
  geom_boxplot()
