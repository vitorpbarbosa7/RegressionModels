y = diamond$price;

x = diamond$carat;

n = length(x)

fit = lm(y ~ x)

summaryobject = summary(fit)

#Soma dos resíduos:
summary(fit)$sigma


# #Como obter a variância a partir dos resíduos -----------------------------------------
e = resid(fit)

sqrt((1/(n-2))*(sum(e^2)))
