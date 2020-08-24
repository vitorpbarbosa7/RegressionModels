library(UsingR)
data(diamond)

y = diamond$price
x = diamond$carat
n = length(x)

beta1 = cor(x,y)*sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)

#Residuos
e = y - beta0 - beta1*x

#Variancia ao redor da linha de regressao
#SSR/df
sigmasquared = sum(e^2)/(n-2)

#Desvio padrao ao redor da linha de regressão
#SRSSR = Square Root Sum of Squares Residuals
sigma = sqrt(sigmasquared)

#Sum of Squares in X
ssx = sum((x - mean(x))^2)

varBeta0 = (1/n + ((mean(x))^2)/ssx)*sigmasquared
varBeta1 = sigmasquared/ssx

seBeta0 = sqrt(varBeta0)
seBeta1 = sqrt(varBeta1)

#Construcao do intervalo de confianca
#Neste teste de hipótese, assume-se que Beta0 e Beta1 são iguais a 0
HBeta0 = 0
HBeta1 = 0

#T calculado:
tBeta0 = (beta0 - HBeta0)/seBeta0
tBeta1 = (beta1 - HBeta1)/seBeta1

#Calculo dos valores p
#O qual significa qual a probablidade obter este valor, mais a probabilidade de obter outro valor igualmente raro, 
#mais a probabilidade de obter outros ainda mais raros
pBeta0 = 2*pt(abs(tBeta0), df = n-2, lower.tail = FALSE)
pBeta1 = 2*pt(abs(tBeta1), df = n-2, lower.tail = FALSE)

#Tabela dos coeficientes
coefTable = rbind(c(beta0, seBeta0, tBeta0, pBeta0),
                c(beta1, seBeta1, tBeta1, pBeta1))

colnames(coefTable) = c("Estimate", "Std.Error", "t value","(P>|t|)")
rownames(coefTable) = c("Beta0","Beta1")

coefTable

#Easy way
fit = lm(y ~ x)
summary(fit)$coefficients

#Intervalo de confiança:
summCoef = summary(fit)$coefficients

#Intervalo de confiança para os coeficientes estimados
#Estamos trabalhando com alpha de signifância igual a 0,05, portanto, 0,05/2 é 0,025
IntervalBeta0 = summCoef[1,1] + c(-1,+1)*qt(.975, df = fit$df)*summCoef[1,2]
IntervalBeta1 = summCoef[2,1] + c(-1,+1)*qt(.975, df = fit$df)*summCoef[2,2]


#Para interpretar de uma melhor maneira o intervalo de confiança, dividimos o coeficiente angular Beta1
#por 10, o que significará que uma mudança em 0.1 quilates (carats) no diamante implicará de A até B de aumento
#no preço do diamente. Podemos afirmar isso com 95 % de confiança

IntervalBeta1/10

#Feito dessa forma, e afirmado desta maneira, confio que em 95% dos casos estarei certo sobre esta afirmaça




# Visualização dos intervalos em gráfico ----------------------------------

library(tidyverse)

#Criacao dos novos dados a partir do intervalo dos origniais com min e max
x = seq(min(x), max(x), length = 100)

#Novo dataframe, novos dados para utilizar na predição
newx = data.frame(x = x)

# Intervalo de confiança, ou seja, intervalo associado a confiança que possuimos nos valores dos 
#parâmetros estimados da regressão linear
p1 = data.frame(predict(fit, newdata = newx, interval = ("confidence")))

# INtervalo de predição, ou seja, intervalo associado à 95 % de confiança que minha predição estará
# em tal intervalo 
p2 = data.frame(predict(fit, newdata = newx, interval  = ("prediction")))

p1$interval = "Confidence"
p2$interval = "Prediction"

p1$x = newx$x
p2$x = newx$x

dat = rbind(p1,p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y )) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) + 
  geom_line() +
  geom_point(data = data.frame(x = diamond$carat, y = diamond$price), aes(x = x, y = y), size = 0.7)
g






























