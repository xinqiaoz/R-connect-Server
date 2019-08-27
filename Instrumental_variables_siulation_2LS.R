library(MASS)

library(parallel)
#Start parallel calculation
##detect the # of cores of computer
cl.cores <- detectCores(logical = F)
cl <- makeCluster(cl.cores)



#Global Variables
#Run and define Global Variales first ! ! !
sigma=matrix(c(25,21,21,36),ncol = 2,byrow = T)


# indp=mvrnorm(n=10000,c(10,50),sigma)
# colnames(indp)=c('X',"Z")
# x=indp[,1]
# z=indp[,2]
# dim(x)=c(length(x),1)
# dim(z)=c(length(z),1)
# cor(x,z)
# cov(x,z)
# 
# y=rep(20,length(x))+5*x+0.3*z+rnorm(length(x),0,1)
# X_r=cbind(rep(1,length(x)),x)
# Beta_OLS=solve(t(X_r) %*% X_r) %*% t(X_r) %*% y
# ERR_OLS = Beta_OLS-c(20,5)
# 
# Z_r=cbind(rep(1,length(z)),z)
# Beta_2LS_1=solve(t(Z_r) %*% Z_r) %*% t(Z_r) %*% x
# X_2LS_1=Z_r %*% Beta_2LS_1
# 
# X_2LS_2=cbind(rep(1,length(X_2LS_1)),X_2LS_1)
# 
# Beta_2LS_2 = solve(t(X_2LS_2) %*% X_2LS_2) %*% t(X_2LS_2) %*% y
# ERR_2LS = Beta_2LS_2 - c(20,5)


#Stimulation on increasing Sample Size to Check Convergency
# ERR_2LS=NULL
# l1=length(100:50000)
# 
# for(N in 100:l1){
#   #Generating independent variables according to bi-normal distribution with mean=10&50, covariance matrix = sigma.
#   indp=mvrnorm(n=N,c(10,50),sigma)
#   x=indp[,1]
#   z=indp[,2]
#   dim(x)=c(length(x),1)
#   dim(z)=c(length(z),1)
#   
#   #Generating Dependent Variable, y = 20 + 5 * x + 0.3 * z + Standard Normal Residuals
#   y=rep(20,length(x))+5*x+0.3*z+rnorm(length(x),0,1)
#   
#   #COnstruct First Step regression for Z
#   #Z_r=cbind(rep(1,length(z)),z)
#   
#   #FIrst Step Regression
#   Beta_2LS_1=solve(t(z) %*% z) %*% t(z) %*% x
#   
#   #Construct X for Second Step of Regression
#   X_2LS_1=z %*% Beta_2LS_1
#   X_2LS_2=cbind(rep(1,length(X_2LS_1)),X_2LS_1)
#   
#   #Second Step Regression
#   Beta_2LS_2 = solve(t(X_2LS_2) %*% X_2LS_2) %*% t(X_2LS_2) %*% y
#   
#   #Record Error
#   ERR_2LS = cbind(ERR_2LS, Beta_2LS_2 - c(20,5))
#   
#   #little Fuction to show the process
#   if(round(x=((N-100)/l1),digits=3)%%0.01==0){
#     print( round(x=((N-100)/l1),digits=3))
#   }
#   
# }
# 
# colnames(ERR_2LS)=c(100:l1)
# plot(x=100:l1,y=ERR_2LS[1,],type = 'l')
# plot(x=100:l1,y=ERR_2LS[2,],type = 'l')

Simulation_2LS = function(){
  ERR_2LS=NULL
  l1=length(100:50000)
  
  for(N in 100:l1){
    #Generating independent variables according to bi-normal distribution with mean=10&50, covariance matrix = sigma.
    indp=mvrnorm(n=N,c(10,50),sigma)
    x=indp[,1]
    z=indp[,2]
    dim(x)=c(length(x),1)
    dim(z)=c(length(z),1)
    
    #Generating Dependent Variable, y = 20 + 5 * x + 0.3 * z + Standard Normal Residuals
    y=rep(20,length(x))+5*x+0.3*z+rnorm(length(x),0,1)
    
    #COnstruct First Step regression for Z
    #Z_r=cbind(rep(1,length(z)),z)
    
    #FIrst Step Regression
    Beta_2LS_1=solve(t(z) %*% z) %*% t(z) %*% x
    
    #Construct X for Second Step of Regression
    X_2LS_1=z %*% Beta_2LS_1
    X_2LS_2=cbind(rep(1,length(X_2LS_1)),X_2LS_1)
    
    #Second Step Regression
    Beta_2LS_2 = solve(t(X_2LS_2) %*% X_2LS_2) %*% t(X_2LS_2) %*% y
    
    #Record Error
    ERR_2LS = cbind(ERR_2LS, Beta_2LS_2 - c(20,5))
    
    #little Fuction to show the process
    if(round(x=((N-100)/l1),digits=3)%%0.01==0){
      print( round(x=((N-100)/l1),digits=3))
    }
    
  }
  
  colnames(ERR_2LS)=c(100:l1)
  plot(x=100:l1,y=ERR_2LS[1,],type = 'l')
  plot(x=100:l1,y=ERR_2LS[2,],type = 'l')
  
}

system.time(Simulation_2LS())

#Close parallel calculation
stopCluster(cl)
