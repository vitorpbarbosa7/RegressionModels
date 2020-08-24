library(UsingR)

data(diamond)

y = diamond$price;
x = diamond$carat;
n = length(y)

#Linear regression
fit = lm(y ~ x)

e = resid(fit)

yhat = predict(fit)

#Dado que y são os valores reais 
#e yhat são os valores obtidos pelo estimador 
#Têm-se que y - hat deve ser igual aos residuos
max(abs(e - (y - yhat)))

#Ao não utilizar a função predict, mas calcular os valores de yhat manualmente, devemos obter 
#o mesmo resultado
max(abs(e - (y - coef(fit)[1] - coef(fit)[2]*x)))

#Residuos nao devem ser correlacionados com o preditor
cov(e,x)

#
sum(e*x)

#Soma dos resíduos deve ser nulo (variar igualmente ao redor do regressor)
sum(e)

#Media zero dos residuos
mean(e)


# Using base R graphics ---------------------------------------------------

plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, 
     frame = F)
abline(fit, lwd = 2)
for (i in 1:n){
  lines(c(x[i],x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)
}


# Melhor visualização para os Residuos ------------------------------------

plot(x, e, 
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,
     frame = F)

abline(h = 0, lwd = 2)

for (i in 1:n){
  lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)
}

# More examples -----------------------------------------------------------

#Números aleatórios entre -3 e 3
x = runif(100, -3, 3)

#Linha de identidade + oscilando com sin(x) e aidcionado ruído com rnorm
y = x + sin(x) + rnorm(100, sd = .2)

#Com esse padrão adicionado de seno, devemos poder ver um comportamento que não é interessante para os residuos?
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) + 
  geom_point(size = 4, colour = "black", alpha = 0.4) + 
  geom_point(size = 2, colour = "red", alpha = 0.4) + 
  geom_smooth(method = "lm", colour ="black")
g

#Plotar os resíduos
#Basicamente temos que mudar o valor da ordenada y
fit = lm(y~x)
e = I(y - coef(fit)[1] - coef(fit)[2]*x)

g = ggplot(data.frame(x = x, y = e), aes(x = x, y = e)) + 
  geom_point(size = 4, colour = "black", alpha = 0.4) + 
  geom_point(size = 2, colour = "red", alpha = 0.4) + 
  geom_hline(yintercept = 0, colour = 'black', size = 2) + 
  ylab("Residual") + 
  xlab("Caret")
g



# Heterocedasticidade -----------------------------------------------------
#Criacao da variavel x 
x = runif(100, 0, 6)
#Criar y que possui heterocedasticidade
#Possui heterocedasticidade porque o desvio padrão não é constante, ele varia
#Se o desvio padrão varia assim, há uma variabilide nos dados não expressa pela modelo de estimador linear 
#Fica explicito que essa variação é 0.001*x nesse caso. 
y = x + rnorm(100, mean =0, sd = 0.001*x)

g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) + 
  geom_point(size = 4, colour = "black", alpha = 0.4) + 
  geom_point(size = 2, colour = "red", alpha = 0.4) + 
  geom_smooth(method = "lm", colour = "black")
g

#A partir do gráfico anterior, parece que o modelo se ajustou perfeitamente aos dados e está tudo certo, 
#poderíamos até dizer que este é um BLUE

#MAAAAAS, ao visualizar os resíduos podemos ver que aquele termo utilizado na construção do desvio padrão de y
#fez toda diferença, dado que com ele, o valor de y apresenta maiores variâncias conforme x cresce
#Isto fere a condição de homocedasticidade
fit = lm(y~x)
e = (y - coef(fit)[1] - coef(fit)[2]*x)

g = ggplot(data.frame(x = x, y = e), aes(x = x, y = e)) + 
  geom_point(size = 4, colour = "black", alpha = 0.4) + 
  geom_point(size = 2, colour = "red", alpha = 0.4) + 
  geom_hline(yintercept = 0, colour = 'black', size = 2) + 
  ylab("Residual") + 
  xlab("Caret")
g


# Com o dataset diamond ---------------------------------------------------

diamond$e = resid(lm(price ~ carat, data = diamond))

g = ggplot(diamond, aes(x = carat, y = e)) + 
  geom_point(size = 4, colour = "blue", alpha = 0.4) + 
  geom_point(size = 2, colour = "red", alpha = 0.4) + 
  geom_hline(yintercept = 0, colour = 'black', size = 2) + 
  ylab("Residual") + 
  xlab("Caret")
g

#Novas maneiras de olhar
#Primeiro termo é um modelo apenas em função de média
#Segundo modelo é está em função dos quilates dos diamantes
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))

fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))

#Representação dos residuos por dotplotds
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 20)
g




























