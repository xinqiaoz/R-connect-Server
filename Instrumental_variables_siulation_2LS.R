library(MASS)

#Global Variables
#Run and define Global Variales first ! ! !
sigma=matrix(c(25,21,21,36),ncol = 2,byrow = T)

indp=mvrnorm(n=10000,c(10,50),sigma)
colnames(indp)=c('X',"Z")
x=indp[,1]
z=indp[,2]
dim(x)=c(length(x),1)
dim(z)=c(length(z),1)
cor(x,z)
cov(x,z)

y=rep(20,length(x))+5*x+0.3*z+rnorm(length(x),0,1)
X_r=cbind(rep(1,length(x)),x)
Beta_OLS=solve(t(X_r) %*% X_r) %*% t(X_r) %*% y
ERR_OLS = Beta_OLS-c(20,5)

Z_r=cbind(rep(1,length(z)),z)
Beta_2LS_1=solve(t(Z_r) %*% Z_r) %*% t(Z_r) %*% x
X_2LS_1=Z_r %*% Beta_2LS_1

X_2LS_2=cbind(rep(1,length(X_2LS_1)),X_2LS_1)

Beta_2LS_2 = solve(t(X_2LS_2) %*% X_2LS_2) %*% t(X_2LS_2) %*% y
ERR_2LS = Beta_2LS_2 - c(20,5)