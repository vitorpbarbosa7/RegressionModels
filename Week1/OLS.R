library(UsingR)

# Resolução do método dos mínimos quadrados 
y = galton$child
x = galton$parent
beta1 = cor(y,x)*sd(y)/sd(x)

beta0 = mean(y) - beta1*mean(x)

# Comparar para ver se efetivamente é o que ocorre:

compare = cbind(c(beta0, beta1), coef(lm(y~x)))

# Que lindo, a resolução da derivada parcial em beta0 e beta1 da expressão de OLS resulta de fato nas equações expostas

coef(lm(y~x))


#Se invertermos, ambos modos (built-in) e por cor e sd resulta no mesmo valor
beta1 = cor(y,x)*sd(x)/sd(y)
beta0 = mean(x) - beta1*mean(y)

compare = cbind(c(beta0, beta1), coef(lm(x~y)))


#Se centrarmos na média 
yc = y - mean(y)
xc = x - mean(x)

#Como os dados já estão centrados na média, têm-se que:
#(Lembre-se dos vídeos de covariância e correlação do statquest)
#É exatamente a expressão de cor(x,y)*sd(y)/sd(x), dado que estes termos são calculados com xc e yc
beta1 = sum(yc*xc)/sum(xc^2)
c(beta1, coef(lm(y ~ x))[2])

# Como fazer regressão na origem sem a intersecção
#Claro que deste modo precisamos já ter os dados centrados na média (yc e xc)
lm(formula = yc ~ xc - 1)


# Com os dados normalizados
yn = (y - mean(y))/sd(y)
xn = (x - mean(x))/sd(x)

#Note que em lm() utilizamos yn e xn (dados normalizados)
cbind(cor(y,x), cor(yn,xn),coef(lm(yn~xn))[2])


 Regressão linear presente no gráfico:

# Regressão linear presente no gráfico ------------------------------------
data('galton')
y = galton$child - mean(galton$child)
x = galton$parent - mean(galton$parent)

#Frequência de ocorrência de cada distância? - Sim, faz sentido, para mesmos pares ordenados as distâncias são iguais
freqData = as.data.frame(table(x,y))
names(freqData) = c("child","parent","freq")

#Porque as.numeric(as.character) ?
freqData$child = as.numeric(as.character(freqData$child))
freqData$parent = as.numeric(as.character(freqData$parent))

g = ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) + 
  scale_size(range = c(1,10), guide = "none") + 
  geom_point(colour = "grey50", aes(size = freq, show_guide = FALSE)) + 
  geom_point(aes(colour = freq, size = freq)) + 
  scale_colour_gradient(low = "lightblue", high = "white") + 
  stat_smooth(method = "lm", formula = y ~ x)
g
