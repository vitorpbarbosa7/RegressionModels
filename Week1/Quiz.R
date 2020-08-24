
# Quiz --------------------------------------------------------------------

#1
x = c(0.18, -1.54, 0.42, 0.95)
w = c(2,1,3,1)

#Média ponderada de x com pesos w
sum(x*w)/(sum(w))



#2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
#Fit the regression through the origin

fit = lm(y ~ x - 1)
coef(fit)

#3
data("mtcars")

fit = lm(mpg ~ wt, mtcars)
coef(fit)[2]

#4
sdy = 1
sdx = 0.5 #Standard deviation one half of the outcome
corxy = 0.5

#Therefore the slope is:
Beta1 = corxy*(sdy/sdx); Beta1


#6
x = c(8.58, 10.46, 9.01, 9.64, 8.86)

xn = (x - mean(x))/sd(x)

#7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

fit = lm(y ~ x)
coef(fit)[1]

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
