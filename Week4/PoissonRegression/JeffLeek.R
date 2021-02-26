setwd('C:/GD/DS/DS_Share/Coursera/RegressionModels/Week4/PoissonRegression')

load('data/gaData.rda')

data = gaData

View(data)

data$julian = julian(data$date)

View(data)

head(data)

plot(data$julian, data$visits, pch = 19, col = 'darkgrey', xlab = 'Julian', ylab = 'Visits')

#Pensando em Linear Regression para modelar este caso

fitlm = lm(data$visits ~ data$julians)
summary(fitlm)
abline(fitlm, col = 'red', lwd = 3)

# Com transformacao de log
fitlog = lm(I(log(data$visits + 1)) ~ data$julian)
summary(fitlog)

coef = coef(fitlog)
expcoef = exp(coef)
expcoef = round(expcoef, 5)
expcoef

#Aumento de 0,2 % de aumento de trafico por dia

#Poisson Regression Fitted
plot(data$julian, data$visits, pch = 19, col = 'darkgrey', xlab = 'Julian', ylab = 'Visits')
glm1 = glm(data$visits ~ data$julian, family = 'poisson')
abline(fitlm, col = 'red', lwd = 3);
#Poisson Regression
lines(data$julian,glm1$fitted.values, col = 'blue', lwd = 3)

#Residual
plot(glm1$fitted.values, glm1$residuals, pch = 19, 
     col = 'grey', ylab = 'Residuals', xlab = 'Fitted')


#Com taxas:
head(data)

glm2 = glm(data$simplystats ~ data$julian, offset = log(visits + 1), 
           family = 'poisson', data = data)
summary(glm2)
plot(data$julian, glm2$fitted.values, col = 'blue',pch = 19, xlab = 'Date', ylab = 'Fitted Counts')
points(data$julian, glm1$fitted.values, col = 'red', pch = 19)

