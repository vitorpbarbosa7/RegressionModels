require(datasets)

data("InsectSprays")

library(tidyverse)

#Cirandagem
g = ggplot(data = InsectSprays, 
           aes( y = count, x = spray, fill  = spray)) + 
  geom_violin(colour = "black", size = 2) + 
  xlab("Type of spray") + ylab("Insect count")
g

#Boxplot é melhor né
g = ggplot(data = InsectSprays, 
           aes( y = count, x = spray, fill  = spray)) + 
  geom_boxplot() + 
  xlab("Type of spray") + ylab("Insect count")
g


# Fit linear model  -------------------------------------------------------
#Estas estimativas são exatamente a média
summary(lm(count ~ spray, data = InsectSprays))$coef #Aqui a referência do teste de hipótese é o sprayA (Beta0)
#Beta0 + Beta1 = média de sprayB
#Beta0 + Beta2 = média de sprayC

## Hard coding the dummy variables manually
# Ao omitir spray == 'A', estou dizendo que essa é minha referência
# O que R está fazendo por trás
summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) + 
             I(1 * (spray == 'F')), 
        data = InsectSprays))$coef

#Com 6 médias é impossível tentar dar fit em 7 parâmetros
summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) + 
             I(1 * (spray == 'F')) + I(1 * (spray == 'A')), 
           data = InsectSprays))$coef

# Removendo a intersecção, ele não considera nenhum como referência (que seria o primeiro nível do fator spray)
summary(lm(count ~ spray - 1, data = InsectSprays))$coef #Aqui a referência do teste de hipótese é 0, ou seja, ver se cada média é ou não diferente de 0

#O resultado é igual a média
summarise(group_by(InsectSprays, spray), mean = mean(count))

levels(InsectSprays$spray)
str(InsectSprays$spray)

# With relevel, C will be the first level, and so the reference
#Intersecção será a média de C
spray2 = relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef
