library(MASS)

?shuttle

data = shuttle

levels(data$wind)

data$wind = relevel(data$wind, ref = 'tail')
levels(data$wind)

lr = glm(use ~ wind, family = 'binomial', data = data)

summary(lr)

coef(lr)

exp(coef(lr)[2])

#2
levels(data$wind)
lr2 = glm(wind ~ magn, family = 'binomial', data = data)

summary(lr2)  

coef(lr2)

exp(coef(lr2))

#3
model1 = glm(use ~ wind, family = 'binomial', data = data)
model2 = glm(1-use~ wind, family = 'binomial', data = data)


#4
data2 = InsectSprays
View(data2)

data2$spray = relevel(data2$spray, ref = 'B')

poisson1 = glm(count ~ spray, family = 'poisson', data = data2)
summary(poisson1)
#Spray A é a referencia

exp(coef(poisson1)[2])

#6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
