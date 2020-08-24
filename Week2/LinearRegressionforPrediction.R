library(UsingR)

data("diamond")
library(tidyverse)

g = ggplot(diamond, aes(x = carat, y = price)) + 
  xlab('Mass (carats)') + 
  ylab ("Price (SIN $)") + 
  geom_point(size = 6, colour = 'black', alpha = 0.2) + 
  geom_point(size = 5, colour = 'blue', alpha = 0.2) + 
  geom_smooth(method = "lm", colour  = 'black')

g

#Regression
fit = lm(price ~ carat, diamond)
summary(fit)

#Let's mean center the carat, so that the intercept is more in one interpretable scale

# Ao centrar os valores da massa, têm-se uma interseccção que nos diz qual o valor em Dólares de Singapura
# de um diamante de massa média de 
mean(diamond$carat)

fit2 = lm(price ~ I(carat - mean(carat)), diamond)
coef(fit2)


#Um aumento de 1 carat (1 quilate, é na realidade muito alto)
#Podemos multiplicar x por 10 para obter um coeficiente 10 vezes menor

fit3 = lm(price ~ I(carat*10 - mean(carat*10)), diamond)
coef(fit3)
#Deste modo, um aumento de 0,1 quilates resulta em aumento no preço de 
# 372,1025 dólares de singapura

#Prediction:
newx = c(0.16, 0.27, 0.34)
#Manual:
coef(fit)[1] + coef(fit)[2]*newx
#Função predict
predict(fit, newdata = data.frame(carat = newx))
