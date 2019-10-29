y = c(-2,-1,0,1.5,2.5)

l = function(x){
  return(sum(2*(y-x)/(1+(y-x)^2)))
}
x=seq(-2,2,0.001)
lo = sapply(x,l)
plot(y = lo,x = x)


lo = cumprod(lo)
x[which(lo<0)]

th = -0.138


1/sum(-2*(1+(y-th)^2)+4*(y-th)^2/(1+(y-th)^2))
curve(dnorm(x,mean = -0.138,sd = sqrt(0.03781204)),from = -1,to = 1)


dens = function(y,th){
  dens0 = NULL
  for(i in 1:length(th)){
    dens0 = c(dens0, 0.25*prod(dcauchy(y,location = th[i],scale = 1)))
  }
  return(dens0)
}

y = c(-2,-1,0,1.5,2.5)

m = 100
th = seq(0,4,1/m)
de = dens(y = y,th = th)
plot(y = de,x = th,type = "l",xlab = "Theta",ylab = "Unnormalized Density")
de.n = de/0.25*sum(de)
plot(y = de.n,x = th,type = "l",xlab = "Theta",ylab = "Normalized Density")



le = 1000

y = c(16,9,10,13,19,20,18,17,35,55)
sy = sum(y)
c = c(58,90,48,57,103,57,86,112,273,64)
n = y+c
sn = sum(n)
al = seq(0,7,length=le)
be = seq(1,30,length=le)

grid = matrix(nrow = le,ncol = le)
for(i in 1:le){
  for(j in 1:le){
    #print(c(i,j))
   grid[i,j] = prod(choose(n,y)*beta(y+al[i],n+be[j]-y)/beta(al[i],be[j]))*(al[i]+be[j])^-2.5
  }
}

grid = grid/sum(grid)
image(al, be, grid)
contour(al, be, grid,add = T)
