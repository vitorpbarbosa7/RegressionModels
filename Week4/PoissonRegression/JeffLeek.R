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

#


