library(tidyverse)
library(magrittr)

# get all the name of all the files ends with ".csv"
a = list.files("D:\\R-connect-Server\\Fidelity",pattern = "csv")
# substitute the ".csv" with blank
b = gsub(pattern = ".csv","",x = a)
# checked the name of files to ensure that they are syntactically valid 
c = setNames(a,make.names(b))
# read all the files and add them into current global environment, also only keep columns "Date" and "Adj.Close", delete missing values
list2env(lapply(c,function(x){read.csv(x) %>% select(Date,Adj.Close) %>% na.omit %>% mutate(x = Adj.Close)}),envir = .GlobalEnv)

FCRDX$Date = as.Date(FCRDX$Date,"%Y-%m-%d")

library(readxl)
Merrill.Lynch.Global.Bond <- read_excel("Indices.xlsx", sheet = "Merrill Lynch Global Bond ",skip = 1)
Merrill.Lynch.Global.Bond = drop_na(Merrill.Lynch.Global.Bond)

Merrill.Lynch.Global.Bond$Date = as.Date(Merrill.Lynch.Global.Bond$Date,"%Y-%m-%d")

hist(Merrill.Lynch.Global.Bond$PX_LAST,breaks = 100)
data = left_join(FCRDX,Merrill.Lynch.Global.Bond,by = "Date")
data = drop_na(data)
abline(coef = c(0,1))
library(tidyverse)

data= data %>% mutate(Adj.Close = (Adj.Close-min(Adj.Close))/(max(Adj.Close)-min(Adj.Close)))
data= data %>% mutate(PX_LAST = (PX_LAST-min(PX_LAST))/(max(PX_LAST)-min(PX_LAST)))

a = data$Adj.Close
b = data$PX_LAST

a.s = sample(a,size = 500,replace = T)
b.s = sample(b,size = 500,replace = T)
sum(a.s>b.s)/length(a.s)
a.ss = sample(a,size = 500,replace = T)
sum(a.s>a.ss)/length(a.s)

###############################################################################

n = 1000
x.1 = runif(n,min = 10,max = 25)
x.2 = rnorm(mean = 3,sd = 3,n = n)
x.3 = rpois(n = n,lambda = 5)
y1 = 5 + 4.4*x.1[1:(n/2)] - 3.5*x.2[1:(n/2)] + 3.3*x.3[1:(n/2)] + rnorm(n/2)
y2 = 2 + 8*x.1[(n/2+1):n] + 3.5*x.2[(n/2+1):n] + 2*x.3[(n/2+1):n] + rnorm(n/2)
y = c(y1,y2)

lm1 = lm(y1~x.1[1:(n/2)]+x.2[1:(n/2)]+x.3[1:(n/2)])
coef(lm1)
lm2 = lm(y2~x.1[(n/2+1):n]+x.2[(n/2+1):n]+x.3[(n/2+1):n])
coef(lm2)
lm3 = lm(y~x.1+x.2+x.3)
summary(lm3)

sc = sum(lm3$residuals^2)
s1 = sum(lm1$residuals^2)
s2 = sum(lm2$residuals^2)
n1 = n/2
n2 = n/2
k = 3
f.test = ((sc-(s1+s2))/k)/((s1+s2)/(n1+n2-2*k))
pf(df1 = k,df2 = (n1+n2-2*k),q = f.test,lower.tail = F)

###############################################################################


library(ggplot2)
ggplot()+geom_line(aes(y = quantile(data$Adj.Close,seq(0,1,0.01)),x = quantile(data$PX_LAST,seq(0,1,0.01))))+
  geom_abline(slope = 1,intercept = 0)
data = mutate(data,log.return = c(1,diff(log(data$Adj.Close))*100))
data = mutate(data,log.return.index = c(1,diff(log(data$PX_LAST))*100))
hist(data$log.return,breaks = 100,probability = T )
plot(x = data$Date,y = data$Adj.Close,type = "l")
plot(x = data$Date,y = data$PX_LAST,type = "l")
