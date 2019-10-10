br = c(16,9,10,13,19,20,18,17,35,55)
vr = c(58,90,48,57,103,57,86,112,273,64)
sum(br)
sum(vr)

theta_r = rbeta(1000,shape1 = 213,shape2 = 949)
hist(theta_r,breaks = 50,probability = T)
t = seq(0.05,0.27,0.001)
lines(t,dbeta(t,shape1 = 213,shape2 = 949))

bn = c(12,1,2,4,9,7,9,8)
vn = c(113,18,14,44,208,67,29,154)
sum(bn)
sum(vn)

theta_n = rbeta(1000,shape1 = 53,shape2 = 648)
hist(theta_n,breaks = 50,probability = T)
t = seq(0.05,0.27,0.001)
lines(t,dbeta(t,shape1 = 53,shape2 = 648))

diff = theta_r-theta_n
hist(diff,probability = T, breaks = 50)


year = seq(1,10,1)
fa = c(24,25,31,31,22,21,26,20,16,22)

pos = function(alpha, beta){
  po = 1
  for(i in year){
    po = po * (((alpha + beta * i)^fa[i] * exp(-alpha-beta*i))/factorial(fa[i]))
  }
  return(po) 
}



m = 100
alpha = seq(21,38,length=m)
beta = seq(-2.3,0.5,length=m)
re = matrix(nrow = m, ncol = m)
for(i in 1:m){
  for(j in 1:m){
    re[i,j] = pos(alpha = alpha[i],beta = beta[j])
  }
}
image(alpha, beta, re)
contour(alpha, beta, re, add = TRUE)
## Find the MLE Estimator alpha_m, beta_m
alpha_m = alpha[which(re==max(re),arr = T)[1]]
beta_m = beta[which(re==max(re),arr = T)[2]]

ind_ab <- matrix(1:(m ^ 2), nrow = m)
sample_ab <- function (ns) {
  is <- sapply(sample.int(m ^ 2, ns, replace = TRUE, prob = re),
               function (ind) which(ind_ab == ind, arr = TRUE))
  cbind(alpha[is[1,]], beta[is[2,]])
}

sam = sample_ab(1000)

als = sam[,1]
bes = sam[,2]


lambda = als + bes * 11
hist(lambda,breaks = 50,probability = T)
fc = matrix(nrow = 1000,ncol = 100)
for(i in 1:length(lambda)){
  fc[i,] = rpois(100,lambda = lambda[i])
}

quantile(fc,probs = c(0.025,0.975))

alphap = seq(10,47,length=m)
betap = seq(-1.35,-0.48,length=m)
rr = matrix(nrow = m, ncol = m)
for(i in 1:m){
  for(j in 1:m){
    rr[i,j] = dnorm(alphap[i],mean = 28.8667,sd = 2.7494^2)*dnorm(betap[j],mean = -0.9212,sd = 0.4421^2)
  }
}
image(alphap, betap, rr)
contour(alphap, betap, rr, add = TRUE)
