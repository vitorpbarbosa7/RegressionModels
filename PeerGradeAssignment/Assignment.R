data = mtcars

View(data)

library(tidyverse)

#Carros que possuem menos cilindros possuem maior eficiencia, consequentemente fazem mais milhas por galao
ggplot(data, aes(x = factor(cyl), y = mpg)) + 
  geom_boxplot(aes(fill = factor(cyl)), alpha = 0.3) + 
  theme_bw()

ggplot(data, aes(x = factor(cyl), y = hp)) + 
  geom_boxplot(aes(fill = factor(cyl)), alpha = 0.3) + 
  theme_bw()

ggplot(data, aes(x = factor(cyl), y = qsec)) + 
  geom_boxplot(aes(fill = factor(cyl)), alpha = 0.3) + 
  theme_bw()

ggplot(data, aes(x = factor(am), y = mpg)) + 
  geom_boxplot(aes(fill = factor(am)), alpha = 0.3) + 
  theme_bw()

#Teste de hipotese para verificar a diferenca de media entre am para mpg
t.test(mpg ~ factor(am), data = data)

#Regressao logistica para modelar mpg em funcao do regressor categórico binário am
lr = glm(am ~ mpg, family = 'binomial', data = data)
summary(lr)
plot(lr)
               