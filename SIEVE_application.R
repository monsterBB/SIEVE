rm(list = ls())
library("np")
library(MASS)
library('fda')
library(basefun)
library("KernSmooth")

data("cps71")
X = cps71$age
Y = cps71$logwage

#linear regression
model.lr <- lm(logwage ~ age, data = cps71)
summary(model.lr)

#Parametric model
model.par <- lm(logwage ~ age + I(age^2), data = cps71)
summary(model.par)

#Kernel estimation
model.np <- npreg(logwage ~ age, 
                  regtype = "ll",
                  bwmethod = "cv.aic",
                  gradients = TRUE,
                  data = cps71)
summary(model.np)

#local polynomial estimation
model.lp <- locpoly(X, Y, degree = 5, kernel = "normal", bandwidth = 20)


#B-spline estimation
basis_fun = function(degr){
ra         = range(X)#boundary knots
numk       = 3 #number of inner knots
mid        = seq(ra[1], ra[2], length.out = numk +2)[2: (numk+1)]#inner knots(quantiles)
deg        = degr #degree
numb       = 3+ deg + 1


basis = bs(X, df = NULL, knots = mid, degree = deg, intercept = T,  Boundary.knots = range(X))
return(basis)
}


basis1 = basis_fun(3)
basis2 = basis_fun(10)
basis3 = basis_fun(25)
model.Bspline1 = lm ( logwage ~ basis1, data = cps71)
model.Bspline2 = lm ( logwage ~ basis2, data = cps71)
model.Bspline3 = lm ( logwage ~ basis3, data = cps71)

#plot basis of degree=3
plot(X,basis1[,1],"l")
lines(X,basis1[,2])
lines(X,basis1[,3])
lines(X,basis1[,4])
lines(X,basis1[,5])
lines(X,basis1[,6])
lines(X,basis1[,7])


#polynomial basis
pol_fun = function(degree_pol){
array_pol = array(0, dim = c(degree_pol, length(X)))
for (i in 0:degree_pol){
  if (i+1<= degree_pol){
    array_pol [i+1,] = X^i
  }
}
basis_pol = t(as.matrix(array_pol))  
return(basis_pol)
}

basis_pol1 =  pol_fun(4)
basis_pol2 =  pol_fun(6)
basis_pol3 =  pol_fun(8)

model.pol1 = lm ( logwage ~ basis_pol1, data = cps71)
model.pol2 = lm ( logwage ~ basis_pol2, data = cps71)
model.pol3 = lm ( logwage ~ basis_pol3, data = cps71)

#plot the results
par(mfrow = c(1,1))
plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.9)
lines(cps71$age, fitted(model.lr),  col = "blue", lwd = 2, main = "linear regression")
lines(cps71$age, fitted(model.np),  col = "red", lwd = 2, main = "Kernel smoother" )
lines(model.lp,  col = " green", lwd = 2, main = "local polynomial fitting")
#lines(cps71$age, fitted(model.par), lty = 1, col = " orange", lwd = 2, main = "parametric model")
#legend(22,15, c("Linear regression","Kernel estimation","parametric estimation"),lty = c(1,2,4) ,fill=c("blue","red","black"),cex=0.8)

plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.9)
lines(cps71$age, fitted(model.Bspline3), lty = 1, col = "green", lwd = 2)
lines(cps71$age, fitted(model.Bspline2), lty = 1, col = "blue", lwd = 2)
lines(cps71$age, fitted(model.Bspline1), lty = 1, col = "red", lwd = 2)
legend(22,15, c("D=3","D=10","D=25"),lty = 1 ,fill=c("red","blue","green"),cex=0.8)


plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.9)
lines(cps71$age, fitted(model.pol1), lty = 1, col = "red", lwd = 2)
lines(cps71$age, fitted(model.pol2), lty = 1, col = "blue", lwd = 2)
lines(cps71$age, fitted(model.pol3), lty = 1, col = "green", lwd = 2)
legend(22,15, c("D=4","D=6","D=8"),lty = 1 ,fill=c("red","blue","green"),cex=0.8)

#################################################################################

