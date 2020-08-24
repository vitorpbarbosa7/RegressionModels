#1

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
n = length(x)

#Calcular na mão este P-Valor para aprender

Beta1 = cor(x,y)*sd(y)/sd(x)
Beta0 = mean(y) - Beta1*mean(x)

#Residuos
e = y - Beta0 - Beta1*x

#Média dos resíduos ao quadrado (Média porque divide por -2)
sigmasquared = sum((e^2)/(n-2))

#Desvio padrão médio dos resíduos
sigma = sqrt(sigmasquared)

#Sum of Squares of X
ssx = sum((x - mean(x))^2)

#Valor que segue a distribuição t de student para o teste de hipótese
H_nula_Beta1 = 0

varBeta1 = sigmasquared/ssx

#Stardard Error Beta1
se_Beta1 = sqrt(varBeta1)

t_calculado = (Beta1 - H_nula_Beta1)/se_Beta1

pBeta1 = 2*pt(abs(t_calculado), df = n-2, lower.tail = FALSE)



# Questão 3 ---------------------------------------------------------------
mtcars = mtcars

fit = lm(mpg ~ wt, data = mtcars)

x = mtcars$wt
y = mtcars$mpg

prediction_interval = data.frame(predict(fit, newdata = newx, interval = ("prediction")))

newx = data.frame(x = mean(x))
names(newx)[1] = "wt"
predict(fit, newdata = newx, interval = ("confidence"))

# 5
newx = data.frame(x = 3)
names(newx)[1] = "wt"
predict(fit, newdata = newx, interval = ("prediction"))


library(tidyverse)

g = ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "black")
g

#6
fit = lm(mpg ~ I(wt/2), mtcars)
newx = data.frame(x = 3)
names(newx)[1] = "wt"
predict(fit, newdata = newx, interval = ("confidence"))

confint(fit)


#9
x = mtcars$wt
y = mtcars$mpg

Beta1 = cor(x,y)*sd(y)/sd(x)
Beta0 = mean(y) - Beta1*mean(x)

#Residuos
e = y - Beta0 - Beta1*x

#Residuos não explicados pelo modelo
SSnoex = sum(e^2)

#Residuos totais
SStot = sum((y - mean(y))^2)

#Razão entre os dois
SSnoex/SStot
