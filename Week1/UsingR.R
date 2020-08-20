library(UsingR)


# Carregar os dados -------------------------------------------------------
#Dados utilizados por Galton em 1885
data("galton")

library(reshape)

long = melt(galton)

g = ggplot(long, aes(x = value, fill = variable)) +
  geom_histogram(colour = "black", binwidth = 1) + 
  facet_grid(.~variable)


