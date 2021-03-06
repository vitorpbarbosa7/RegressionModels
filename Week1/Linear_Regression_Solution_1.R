## Regression through the origin
# Suppose that $X_i$ are the parents heights.
# * Consider picking the slope $\beta$ that minimizes $$\sum_{i=1}^n (Y_i - X_i \beta)^2$$
# * This is exactly using the origin as a pivot point picking the
# line that minimizes the sum of the squared vertical distances
# of the points to the line
# * Use R studio's  manipulate function to experiment
# * Subtract the means so that the origin is the mean of the parent
# and children's heights

---
# ```{r, echo = TRUE, eval = FALSE}

#Computar as dist�nicas    
  
library(UsingR)

data('galton')
y = galton$child - mean(galton$child)
x = galton$parent - mean(galton$parent)

#Frequ�ncia de ocorr�ncia de cada dist�ncia? - Sim, faz sentido, para mesmos pares ordenados as dist�ncias s�o iguais
freqData = as.data.frame(table(x,y))
names(freqData) = c("child","parent","freq")

#Porque as.numeric(as.character) ?
freqData$child = as.numeric(as.character(freqData$child))
freqData$parent = as.numeric(as.character(freqData$parent))

library(tidyverse)
myplot = function(beta){
  g = ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) + 
    scale_size(range = c(1,10), guide = "none") + 
    geom_point(colour = "grey50", aes(size = freq, show_guide = FALSE)) + 
    geom_point(aes(colour = freq, size = freq)) + 
    scale_colour_gradient(low = "lightblue", high = "white") + 
    geom_abline(intercept = 0, slope = beta, size = 3)
  
    mse = mean((y - beta * x)^2)
  
    g = g + ggtitle(paste("beta = ", beta, "mse = ", round(mse,3)))
    g
}
  
library(manipulate)
manipulate(myplot(beta), beta = slider(0.0, 2, step = 0.01))

  
# Afinal, qual � o beta que minimiza os erros quadr�ticos? ----------------

estimator = lm(child ~ parent, galton)

summary(estimator)

plot(estimator)

mse = mean(estimator$residuals^2)


# Solu��o pela forma do curso, visualizando o deslocamento do eixo por centrar os dados na m�dia --------
#Dados centrados na m�dia

centeredmodel = lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)


