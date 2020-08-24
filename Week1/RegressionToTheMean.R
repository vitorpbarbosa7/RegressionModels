
# Regression to the mean example with normal distribuition ----------------

a = rnorm(100)

b = rnorm(100)

aordered = order(a)

a[aordered[100]]

b[aordered[100]]

#Example

library(UsingR)

data(father.son)

#Normalizar
#sheight : son's height
#fheight : father's height

y = (father.son$sheight - mean(father.son$sheight))/sd(father.son$sheight)
x = (father.son$fheight - mean(father.son$fheight))/sd(father.son$fheight)

#Correlação:
rho = cor(x,y)

#Chart
library(tidyverse)

g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) + 
  geom_point(size = 6, colour = "black", alpha = 0.2) + 
  geom_point(size = 4, colour = "salmon", alpha = 0.6) + 
  xlim(-4,4) + ylim(-4,4) + 
  geom_abline(intercept = 0, slope = 1) + #Dados estao centrados na media, normalizados, entao tudo bem 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  geom_abline(intercept = 0, slope = rho, size = 2) + # child ~ parent (y ~ x)
  geom_abline(intercept = 0, slope = 1/rho, size = 2) # parent ~ child (x ~ y)

g
  
  
  














