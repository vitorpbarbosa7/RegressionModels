require(datasets)

data(swiss)

#São poucos dados
library(tidyverse)

#Addon tools for ggplot
library(GGally)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...)
    #geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(swiss, lower = list(continuous = my_fn))
g


# Investigar as relações por regressão linear  ----------------------------

# Com todas variáveis:
coeftable = summary(lm(Fertility ~ ., data = swiss))$coefficients
coeftable

#Teste de hipótese para o primeiro coeficiente
tcalc_Beta1 = (coeftable[2,1] - 0)/coeftable[2,2]
pBeta1 = 2*pt(abs(tcalc_Beta1), df = length(swiss$Agriculture)-ncol(swiss), lower.tail = FALSE)
pBeta1

coeftable


# Com 1 variável apenas ---------------------------------------------------
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

# Ao fazer pelos resíduos: (MUITO INTERESSANTE!!)
ey = resid(lm(Fertility ~ Examination + Education + Catholic + Infant.Mortality, data = swiss))
ex = resid(lm(Agriculture ~ Examination + Education + Catholic + Infant.Mortality, data = swiss))

sum(ey*ex)/sum(ex^2)


# Simulation  -------------------------------------------------------------
n  = 100
x2 = 1:n #Time
x1 = .01*x2 + runif(n, -.1, .1); #Money

y = -x1 + x2 + rnorm(n, sd = .01) #Happiness

#Podemos ver como y está positivamente correlacionado com x neste gráfico? NÃÃO!!!!!!!!
# O que está acontecendo é que y está com a influência de x2, e essa influência positiva gigantesca de x2 (comparando x2 com x1)
# não está sendo expressa neste gráfico abaixo
# Logo esta correlação está ERRADA!!!!!!!!!!
ggplot(data.frame(x = x1, y = y), aes(x = x1, y = y)) + geom_point()

data = data.frame("x1" = x1, "x2" = x2, "y" = y)
g = ggpairs(data, lower = list(continuous = my_fn))
g
# De fato visualizamos duas correlações, positivas e para x1 sabemos que está errado?
# Se for isso é bem tenso

# Voltando ao modelo criado 
summary(lm(y ~ x1 + x2))$coef


# Plots  ------------------------------------------------------------------
g = ggplot(data, aes(y = y, x = x1)) + 
  geom_point(aes(colour = x2), size = 5) + 
  scale_colour_gradient2(low = "green", mid = "blue", high = "red", midpoint = mean(data$x2))
  #geom_smooth(method = lm, se = FALSE, colour = "black")
g

#Criando residuos (retirando o efeito de x2 para ver a correlação negativa com x1)
#x2 que dava a correlação positiva, então estou retirando ela 
data$ey = resid(lm(y ~ x2))
data$ex1 = resid(lm(x1 ~ x2))

#Esse é o gráfico real do efeito de x1 em y, dado que retiramos o efeito de x2 que estava mascarando tudo 
ggplot(data, aes(x = ex1, y = ey)) + 
  geom_point()


# O que acontece se adicionarmos variáveis linearmente dependentes --------
z = swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)
#NA coefficient (problema na inversão da Design Matrix?)

ggpairs(data, lower = list(continuous = my_fn))
