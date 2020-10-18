#Lib
library(car)

fit = lm(Fertility ~ ., data = swiss)

#Se fosse 1, seriam todos ortogonais aos outros 
#Na realidade então não temos vetores coluna sempre ortogonais? 
vif(fit)
#Infant.Mortality is unrelated to the other variables

#PCA éh util para reduzir a covariancia espacial 
#Reduz a interpretabilidade

#Nested models
fit1 = lm(Fertility ~ Agriculture, data = swiss)
fit3 = update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 = update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)

anova(fit, fit3, fit5)
