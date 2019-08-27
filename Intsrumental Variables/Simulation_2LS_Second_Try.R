library(MASS)

sigma=matrix(c(25,15,3.6,15,36,0,3.6,0,1),ncol=3)

indp=mvrnorm(n=10000,mu=c(10,50,0),Sigma=sigma)

x=indp[,1]
z=indp[,2]
e=indp[,3]

var(x)
var(z)
var(e)

cor(x,z)
cor(e,z)
cor(x,e)


ERR=NULL
for (i in 1:10000) {
  indp=mvrnorm(n=10000,mu=c(10,50,0),Sigma=sigma)
  
  x=indp[,1]
  z=indp[,2]
  e=indp[,3]
  
  y=rep(20,length(x))+12*x+e
  x_OLS=cbind(1,x)
  Beta_OLS=solve(t(x_OLS)%*%x_OLS)%*%t(x_OLS)%*%y
  ERR=c(ERR,Beta_OLS[2,]-12)
}

# y=rep(20,length(x))+12*x+e
# x_OLS=cbind(1,x)
# 
# Beta_OLS=solve(t(x_OLS)%*%x_OLS)%*%t(x_OLS)%*%y
# 
# z_2LS=cbind(1,z)
# 
# Beta_2LS_1=solve(t(z_2LS)%*%z_2LS)%*%t(z_2LS)%*%x
# x_2LS=z_2LS%*%Beta_2LS_1
# x_2LS=cbind(1,x_2LS)
# 
# Beta_2LS=solve(t(x_2LS)%*%x_2LS)%*%t(x_2LS)%*%y


ERR=NULL
for (i in 1:10000) {
  indp=mvrnorm(n=10000,mu=c(10,50,0),Sigma=sigma)
  
  x=indp[,1]
  z=indp[,2]
  e=indp[,3]
  
  y=rep(20,length(x))+12*x+e
  
  z_2LS=cbind(1,z)
  Beta_2LS_1=solve(t(z_2LS)%*%z_2LS)%*%t(z_2LS)%*%x
  
  x_hat=z_2LS %*% Beta_2LS_1
  
  x_OLS=cbind(1,x_hat)
  
  Beta_OLS=solve(t(x_OLS)%*%x_OLS)%*%t(x_OLS)%*%y
  ERR=c(ERR,Beta_OLS[2,]-12)
}

plot(ERR)
hist(ERR)
