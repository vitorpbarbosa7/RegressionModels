
# 1 -----------------------------------------------------------------------
data = mtcars

data$cyl = as.factor(data$cyl)

fit = lm(mpg ~ cyl + wt, data = data)

summary(fit)

library(tidyverse)

# Da mudanca esperada de carros com cilindros de 4 e cilindros de 8, tem-se uma mudanca
#de -6,0709, a qual corresponde a diferenca de media entre os dois
g = ggplot(data = data, aes(y = mpg, x = cyl)) + 
  geom_point()

g2 = ggplot(data = data, aes(y = mpg, x = wt)) + 
  geom_point(aes(fill = cyl, color = cyl))
g2
# 2 -----------------------------------------------------------------------
data = mtcars 

data$cyl = as.factor(data$cyl)

fit2 = lm(mpg ~ cyl + wt, data = data)
summary(fit2)

g2 = g2 + geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[4], color = 'red')
g2 = g2 + geom_abline(intercept = coef(fit2)[1] + coef(fit2)[2], slope = coef(fit2)[4], color = 'green')
g2 = g2 + geom_abline(intercept = coef(fit2)[1] + coef(fit2)[3], slope = coef(fit2)[4], color = 'blue')
g2

#Realizar o fit com o 4 sem ser a referencia, assim ele literalmente traz a media dos tres, muito lindo 
fit2semreferencia = lm(mpg ~ wt + cyl - 1, data = data)
summary(fit2semreferencia)  

#wt constante
fit3 = lm(mpg ~ cyl, data = data)
summary(fit3)

g3 = ggplot(data = data, aes(y = mpg, x = cyl)) + 
  geom_point()
g3

g3 = g3 + geom_abline(intercept = coef(fit3)[1], slope = 0, color = 'black')
g3 = g3 + geom_abline(intercept = coef(fit3)[1] + coef(fit3)[2],slope = 0, color = 'red')
g3 = g3 + geom_abline(intercept = coef(fit3)[1] + coef(fit3)[3], slope = 0, color = 'green')
g3

#Coloquei alternativa 

# Holding weight constant, cylinder appears to have more of an impact on mpg than if weight is disregarded.


# 3 -----------------------------------------------------------------------
data = mtcars

data$cyl = as.factor(data$cyl)

fit4 = lm(mpg ~ cyl + wt, data = data)

fit5 = lm(mpg ~ cyl*wt, data = data)

summary(fit4)

summary(fit5)

#Realizando ANOVA entre fit4 e fit5, tem-se que o fit4 eh a referencia, a qual nao possui o termo de interferencia, 
#Deste modo, sera que a adicao dos termos de referencia eh estatisricamente relevante?
# MUITO BONITO, MUITO BONITO
anova(fit4, fit5)

# 4
data = mtcars

data$cyl = as.factor(data$cyl)

fit6 = lm(mpg ~ I(wt * 0.5) + factor(cyl), data = data)

summary(fit6)

# 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

#Vamos visualizar estes dados neh

hatdata = data.frame(y = y , x = x)

ghat = ggplot(data = hatdata, aes(y = y, x = x)) + 
  geom_point()
ghat

hatfit = lm(y ~ x, data = hatdata)

hatvalues(hatfit)

# 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

dfbetas(hatfit)















