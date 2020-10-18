rm(list=ls())
setwd('C:/GD/DS/DS_Share/Coursera/RegressionModels/Week4')

load("data/ravensData.rda")
head(ravensData)

data = ravensData

#Dado que os dados são binarios, ele assume que a link function que 
#iremos utilizar é a logistic link function
lr = glm(data$ravenWinNum ~ data$ravenScore, family = 'binomial')

summary(lr)

#Plot probabilities

plot(data$ravenScore, lr$fitted.values, 
     pch = 19, col = 'blue', 
     xlab = 'Score', ylab = 'Prob Ravens Win')

#Interpretacao do aumento em x e correspondencia na probabilidade

exp(lr$coefficients)
# Um aumento de 11% de probabilidade dado um aumento de 1 unidade no Score

#Confidence intervals
exp(confint(lr))

#Anova
anova(lr, test = 'Chisq')
