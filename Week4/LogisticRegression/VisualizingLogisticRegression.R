rm(list=ls())

#Visualizing fitting regression curves

x = seq(-10, 10, length = 1000)

library(manipulate)


a = c(-9,-7,-5,-3,-1,1,3,5,7,9)
b = c(0,0,0,1,0,1,0,1,1,1)

plot(a, b)
par(new = TRUE)

manipulate(
  plot(x, 
       exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x)),
       type = "l", lwd = 3, frame = FALSE),
  beta1 = slider(-5, 5, step = .1, initial = 2),
  beta0 = slider(-5, 5, step = .1, initial = 0)
  )

