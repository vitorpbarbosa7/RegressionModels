
# Demonstrar como são gerados os coeficientes da regressão linear  --------

# Geração dos dados
n = 100;
# Criação de 3 distribuições normais
x = rnorm(n);
x2 = rnorm(n);
x3 = rnorm(n);

#Variável y
y = 1 + x + x2 + x3 + rnorm(n, sd = 0.1)

#Resíduo de y ao desconsiderar x1 e x2
ey = resid(lm(y ~ x2 + x3))

#ey é o y sem o efeito de x2 e x3, os quais foram removidos, sobrando apenas seus resíduos
#Ao realizar este procedimento, estamos abrindo mão da explicação que x2 e x3 poderiam fornecer sobre alguma variação sistemática em y associado 
# a x2 e x3 e ficando apenas com o resíduo. 

#Este é um dos primeiros passos para exclusão que permitirá obter o coeficiente exclusivo para B1

#Agora a regressão linear de x em x2 e x3 para obter o resíduo de x ao desconsiderar a explicação de variação sistemática por x2 e x3
# Mas x, x2 e x3 não deveriam ser independentes?

ex = resid(lm(x ~ x2 + x3))

#Regressão pela origem:
#Este é o valor de Beta1 para x, dado que foram retiradas as influências dos outros dois x2 e x3 para ambas variáveis
# Ou seja, estamos utilizando apenas os resíduos
sum(ey*ex)/sum(ex^2)

#Por lm:
coef(lm(ey ~ ex - 1))

#No geral:
coef(lm(y ~ x + x2 + x3))
