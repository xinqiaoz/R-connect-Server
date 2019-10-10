n=30 # sample size (# of experiment)
m=5  # number of trials in each experiment
alpha = 1
beta = 0
#theta = rbeta(1,alpha,beta)
x = rbinom(n,m,0.75) # observations

mod = (mean(x)+(alpha-1)/n)/(m+(alpha+beta-2)/n)
I.theta = (sum(x)+alpha-1)/(mod^2)+(m*n+beta-sum(x)-1)/((1-mod)^2)
sd = sqrt(1/I.theta)

t = seq(0.5,1,0.0005)
plot(t, dbeta(t, sum(x)+alpha, beta + m*n-sum(x)), type = "l",
     xlab = expression(theta), ylab = "density")
lines(t, dnorm(t, mod, sd), lty = 2)
