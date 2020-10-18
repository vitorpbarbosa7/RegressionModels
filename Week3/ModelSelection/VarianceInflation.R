rm(list=ls())
n = 100; nosim = 1000

x1 = rnorm(n); x2 = rnorm(n); x3 = rnorm(n);

betasa = sapply(1 : nosim, function(i){
  y = x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})

round(apply(betasa, 1, sd), 5)

# Now inflation:
x1 = rnorm(n); 
x2 = x1/sqrt(2) + rnorm(n)/sqrt(2);
x3 = x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2);

betasa = sapply(1 : nosim, function(i){
  y = x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})

round(apply(betasa, 1, sd), 5)

