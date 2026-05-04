# Cross-scoring comparaison for Paris

# Import Maria's cross-scoring file

column_names = read_excel(path = "Raw/column_names.xlsx")


## import csv by district ----
cross_maria = read_excel(path = "Raw/Cross scoring Maria.xlsx",
                  skip = 1)

names(cross_maria)[1:35] = column_names$Column_corr

# create id
cross_maria <- cross_maria %>%
  mutate( ID_site = paste(toupper(str_sub(Arr, 1,3)), Sites, sep = "_"))

# compare scores with Katia's new google scoring
cross_UWS <- uws_paris_iris %>%
  left_join( cross_maria, suffix = c("", "_maria"), 
             by = c("ID_site" = "ID_site") )


## Regression lineaire katia vs Maria ----

ggplot(data = cross_UWS,
       mapping = aes(x = `Total wildness`, y = `Total wildness_maria`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  geom_abline(slope = 1, intercept =0 ) +
  ylim (0,5) + xlim (0,5)

## test de correlation de spearman (correlation des rangs) ----
cor.test(cross_UWS$`Total wildness`, 
         cross_UWS$`Total wildness_maria`,
          method = 'spearman')

## test de correlation de spearman (correlation des rangs) ----
summary( cross_reg <- lm(`Total wildness_maria`~`Total wildness`,data = cross_UWS))
plot(simulateResiduals(cross_reg)) # not bad

# R2 = 0.65 ; Equation : UWS_maria =  -0,2703 + 1,0633 * UWS_katia
# Regression not horrible but not very tight.

# Need to check against rescored katia data



