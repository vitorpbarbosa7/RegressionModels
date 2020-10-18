setwd('C:/GD/DS/DS_Share/Coursera/RegressionModels/Week4')

load("data/ravensData.rda")
head(ravensData)

data = ravensData

class(data)

write.csv(data, file = 'data/data.csv')

summary(data)

View(data)

#Linear Regression

lmRavens = lm(data$ravenWinNum ~ data$ravenScore)
summary(lmRavens)
