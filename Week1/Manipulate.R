library(UsingR)


# Carregar os dados -------------------------------------------------------
#Dados utilizados por Galton em 1885
data("galton")

library(reshape)

long = melt(galton)

g = ggplot(long, aes(x = value, fill = variable)) +
  geom_histogram(colour = "black", binwidth = 1) + 
  facet_grid(.~variable)


# Manipulate --------------------------------------------------------------

library(manipulate)
library(tidyverse)

myHist <- function(mu){
  #Erro quadrático médio
  mse <- mean((galton$child - mu)^2)
  
  g <- ggplot(galton, aes(x = child)) +
    geom_histogram(fill = "salmon", colour = "black", binwidth = 1)
  
    g <- g + geom_vline(xintercept = mu, size = 3) 
    g <- g + ggtitle(paste("mu =", mu, ",mse = ", round(mse,2), sep = ""))
  g
}

manipulate(myHist(mu), mu = slider(62,74,step = 0.5))
dev.off()


# Without Function --------------------------------------------------------
mu = 70
mse = mean((galton$child - mu)^2)

g = ggplot(galton, aes(x = child)) +
  geom_histogram(fill = "salmon", colour = "black", binwidth = 1) + 
  geom_vline(xintercept = mu, size = 3) + 
  ggtitle(paste("mu =", mu, ",mse = ", round(mse,2), sep = ""))
g




# Scatterplot -------------------------------------------------------------

ggplot(galton, aes(x = parent, y = child)) + 
  geom_point()





freqData = as.data.frame(table(galton$child, galton$parent))
names(freqData) = c("child","parent","Freq")
freqData$child = as.numeric(as.character(freqData$child))
freqData$parent = as.numeric(as.character(freqData$parent))

g = ggplot(filter(freqData, Freq > 0), aes(x = parent, y = child)) + 
  scale_size(range = c(2,20), guide = "none") + 
  geom_point(colour = 'grey50', aes(size = Freq + 20, show_guide = FALSE)) + 
  geom_point(aes(colour = Freq, size = Freq)) + 
  scale_colour_gradient(low = "lightblue", high = "white")
g
