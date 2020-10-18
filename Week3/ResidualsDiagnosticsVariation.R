data(swiss);
par(mfrow = c(2,2))

fit = lm(Fertility ~ ., data =  swiss);
plot(fit)

#Simulation experiments:
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  

fit = lm(y ~ x)
round(dfbetas(fit)[1:10,2],3)

dfbetas(fit)

hatvalues(fit)

#
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)       


#
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)
```

#
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)

#
summary(lm(V1 ~ . -1, data = dat))$coef

#
```{r, fig.height=4, fig.width=4, echo = TRUE}
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')

#
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)
