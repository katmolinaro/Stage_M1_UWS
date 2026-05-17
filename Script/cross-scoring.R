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



## Comparaison  katia vs Maria ----

# mean absolute difference: 
mean(abs(cross_UWS$`Total wildness_maria`- cross_UWS$`Total wildness`), na.rm = TRUE)


## Paired t-test ----
# mean absolute difference: 
mean(cross_UWS$`Total wildness_maria`- cross_UWS$`Total wildness`, na.rm = TRUE)
# on average scores are -0.13 points less for Maria...

t.test(cross_UWS$`Total wildness_maria`, cross_UWS$`Total wildness`,
       alternative = "less",
       paired = TRUE)
# No significant trend in the difference = no systematic under or over scoring. 

## test de correlation de spearman (correlation des rangs) ----
cor.test(cross_UWS$`Total wildness`, 
         cross_UWS$`Total wildness_maria`,
          method = 'spearman')

## Linear regression (RMS) ----
summary( cross_reg <- lm(`Total wildness_maria`~`Total wildness`, data = cross_UWS))
plot(simulateResiduals(cross_reg)) # not perfect

# mean error:
mean(abs(residuals(cross_reg)))


# R2 = 0.65 ; Equation : UWS_maria =  -0,2703 + 1,0633 * UWS_katia
# Regression not horrible but not very tight.
# Need to check against rescored katia data

summary( cross_reg_0 <- lm(`Total wildness_maria`~0 +`Total wildness`, data = cross_UWS))
plot(simulateResiduals(cross_reg_0)) # not perfect

## SMA (Single major axis estimation) ----
library(smatr)

#### Free intercept :  ----
(cross_sma <- sma(`Total wildness_maria`~ `Total wildness`, data = cross_UWS,
                  slope.test = 1,
                  elev.test = 0))
# slope not diff from 1, Intercept not diff. from 0, but P values are marinal (0.060-0.076)
# Still pretty reassurring.
plot(cross_sma)
plot(cross_sma , which = "residual")



### Regression SMA forced through 0 intercept:----
(cross_sma_0 <- sma(`Total wildness_maria`~ 0 + `Total wildness`,data = cross_UWS))
plot(cross_sma_0)

# Graph ----


## Simple linear regression graph ----
ggplot(data = cross_UWS,
       mapping = aes(x = `Total wildness`, y = `Total wildness_maria`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "grey",
                linetype = "solid", linetype = "dotted") +
  geom_abline(aes(slope =  cross_reg$coefficients[2],
                  intercept =  cross_reg$coefficients[1]),  col = "darkblue") +
  ylim (0,5) + xlim (0,5) +
  geom_text(mapping = aes(x = x, y = y, label = label),
            data = data_frame(x = c(2.5, 2.5), 
                              y = c(1, 0.7), 
                              label = c("R2 = 0.65",
                                        "UWS_maria =  -0,2703 + 1,0633 * UWS_katia")),
            hjust = "left", size =3) +
  theme_minimal()

## Model comparison graph ----
ggplot(data = cross_UWS,
       mapping = aes(x = `Total wildness`, y = `Total wildness_maria`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "grey",linetype = "solid") +
  geom_abline(aes(slope =  cross_reg$coefficients[2],
                      intercept =  cross_reg$coefficients[1],
                      col = "lm",linetype = "free")) +
   geom_abline(aes(slope = cross_sma$groupsummary$Slope,
              intercept = cross_sma$groupsummary$Int,
              col = "sma",linetype = "free")) +
   scale_linetype_manual(name = "Intercept", values = c("free" = "solid", "via0" = "dashed")) + 
  scale_color_manual(name = "Models", values = c("lm" = "darkblue", "sma" = "red")) + 
  ylim (0,5) + xlim (0,5) +
  theme_minimal()
