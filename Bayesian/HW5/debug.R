data = data.frame("y" = c(13,52,6, 40, 10, 7, 66, 10, 10, 14, 16, 4,
                          65, 5, 11, 10, 15, 5, 76, 56, 88, 24, 51, 4,
                          40, 8, 18, 5, 16, 50, 40, 1, 36, 5, 10, 91,
                          18, 1, 18, 6, 1, 23, 15, 18, 12, 12, 17, 3))

y = data$y
mcmc_array <- function (ns, nchains = 1, params) {
  nparams <- length(params)
  array(dim = c(ns, nchains, nparams),
        dimnames = list(iterations = NULL,
                        chains = paste0("chain", 1:nchains),
                        parameters = params))
}

nc = 4
ns = 100000
cs = c("Lambda","Mu","Sigma","Lp__")

la.c = 0.5
mu.c = 3
si.c = 1

la.sd = 0.01
mu.sd = 80
si.sd = 1000

n = length(y)

sims = mcmc_array(ns,nchains = nc, params = cs)

wi = function(la){
  y = data$y
  if(abs(la)<0.005){
    w = log(y)
  } else{
    w = (y^la-1)/la
  }
  return(w)
}

w.mu = function(la,mu){
  w = wi(la = la)
  re = 0-sum((w-mu)^2)
  return(re)
}

mean.mu = function(la){
  w = wi(la = la)
  return(mean(w))
}

si.n = function(si){
  return(si^(-(n+1)/2))
}
for(j in 1:nc){
  for(i in 1:ns){
    la.s = rnorm(n = 1,mean = la.c,sd = la.sd)
    up = sum((la.s-1)*log(y))+w.mu(la = la.s,mu = mu.c)/(2*si.c)
    down = sum((la.c-1)*log(y))+w.mu(la = la.c,mu = mu.c)/(2*si.c)
    r1 = exp(up-down)
    if(r1 >= runif(1)){la.c = la.s}else{la.c = la.c}
    
    mu.s = rnorm(n = 1,mean = mean.mu(la = la.c),sd = si.c/n)
    # mu.s = rnorm(n = 1,mean = mu.c,sd = mu.sd)
    # up = w.mu(la = la.c,mu = mu.s)/(2*si.c)
    # down = up = w.mu(la = la.c,mu = mu.c)/(2*si.c)
    # r2 = exp(up - down)
    # if(r2 >= runif(1)){mu.c = mu.s}else{mu.c = mu.c}
    
    si.s = 1/rgamma(n = 1,shape = (n-1)/2,rate = -w.mu(la = la.c,mu = mu.c)/2)
    # si.s = rnorm(n = 1,mean = si.c,sd = si.sd)
    # if(si.s >0){
    #   up = log(si.s) + w.mu(la = la.c,mu = mu.c)/(2*si.s)
    #   down = log(si.c) + w.mu(la = la.c,mu = mu.c)/(2*si.c)
    #   r3 = exp(up - down)
    #   if(r3 >= runif(1)){si.c = si.s}else{si.c = si.c}
    # }


    lp = 1/sqrt(si.c)*prod(y^la.c*1/(sqrt(2*pi*si.c)))*exp(w.mu(la = la.c,mu = mu.c)/(2*si.c))
    sims[i,j,] = c(la.c,mu.c,si.c,lp)
  }
}
mcmc_trace(sims[95000:100000,,])
