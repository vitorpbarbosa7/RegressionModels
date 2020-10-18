library(datasets)
data(swiss)
head(swiss)

library(dplyr)

#Criar novas variaveis com a funcao mutate do dplyr

#Variavel binaria
swiss = mutate(swiss, CatholicBin = 1*(Catholic > 50))
swiss$CatholicBin = as.factor(swiss$CatholicBin)

#Plot data
library(tidyverse)

g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = CatholicBin)) + 
  geom_point(size = 6, colour = 'black') + geom_point(size = 4) + 
  xlab('% in Agriculture') + 
  ylab('Fertility')
g

# So we can fit a model with Fertility as function of Agriculture and CatholicBin
fit = lm(Fertility ~ Agriculture, data = swiss)
#Extrair intercept and slope from this:
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2) 
g1

#Adicionnando CatholicBin as independent, sem interacao
fit2 = lm(Fertility ~ Agriculture + CatholicBin, data = swiss)
g2 = g
g2 = g2 + geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2], size = 2)
g2 = g2 + geom_abline(intercept = coef(fit2)[1] + coef(fit2)[3], slope = coef(fit2)[2], size = 2, colour = 'blue')
g2

#Com termo de interacao entre Agriculture e CatholicBin utilizamos a multiplicacao entre ambos
fit3 = lm(Fertility ~ Agriculture * CatholicBin, data = swiss)
g3 = g
g3 = g3 + geom_abline(intercept = coef(fit3)[1], slope = coef(fit3)[2], size = 2)
g3 = g3 + geom_abline(intercept = coef(fit3)[1] + coef(fit3)[3], slope = coef(fit3)[2] + coef(fit3)[4], size = 2, colour = 'red')
g3

