library(quantmod)
StoLis=read.csv(file='Stock List.csv',header = FALSE,stringsAsFactors = FALSE)
#load the list of stocks(200 in total)

Log=list()
Nonlog=list()
Cor=list()
NonCor=list()
error=list()

for (i in 1:length(StoLis[,1])){
  STOK=try(getSymbols(StoLis[i,1], from = "2010-01-01", to = "2018-12-1", auto.assign = FALSE),silent = TRUE)
  #get the price from Yahoo
  if('try-error' %in% class(STOK)){
    error[length(error)+1]=StoLis[i,1]
    next
  }
  #Some stock may fail in getting price
  AdjPrice=ts(STOK[,6],frequency = 1,start = 1)
  #get the adjusted price
  Return=vector(mode = 'numeric')
  for(j in 2:length(AdjPrice)){
    Return[j-1]=100*log(AdjPrice[j]/AdjPrice[j-1])
  }
  #Compute the log return
  a=shapiro.test(Return)
  a=a[["p.value"]]
  #run the shapiro test and get the P-value
  if(a<0.05){
    Nonlog[length(Nonlog)+1]=StoLis[i,1]
  }else{
    Log[length(Log)+1]=StoLis[i,1]
  }
  #classify stocks with normal distributed log retuen
  bo=Box.test(Return,lag=(length(Return)/4),type = 'Ljung-Box',fitdf = 0)
  #run the Ljung-Box test and get the P-value
  if(bo$p.value<0.05){
    Cor[length(Cor)+1]=StoLis[i,1]
  }else{
    NonCor[length(NonCor)+1]=StoLis[i,1]
  }
  #classify stocks if log return is correlated with the past
}

library(vars)
library(fGarch)
library(TSA)
library(tseries)
library(stargazer)
library(akima)
library(stargazer)
library(forecast)
library(stats)
library(urca)
library(rugarch)
library(zoo)
library(fUnitRoots)

STOK<- getSymbols("AAPL", from = "2010-01-01", to = "2018-12-01", auto.assign = FALSE)
AdjPrice=ts(STOK[,6],frequency = 1,start = 1)
#get the adjusted price

return=vector(mode = 'numeric')
for(i in 2:length(AdjPrice)){
  return[i-1]=100*log(AdjPrice[i]/AdjPrice[i-1])
}
#compute the log return
plot(LowPrice,type='l')
RETPLOT<-plot(return,type='l')
RETHIST<-hist(return,breaks = 250)
#plot regard to log return

shapiro.test(return)
#Shapiro normality test
jbTest(return)
#JB normality test

adf.test(return,alternative = 'stationary')
#ADF test for stationary

pp.test(return,alternative = 'stationary',type = 'Z(t_alpha)')
#PP test for stationary

kpss.test(return)
#KPSS test for stationary

pac=pacf(return,lag.max = 20)
ac=acf(return,lag.max = 20)
#plot the ACF anf PACF fuction to help to determine the lag of model

a1=arima(return,order = c(7,0,4))
a2=arima(return,order = c(4,0,8))
a3=arima(return,order = c(0,0,11))
a4=arima(return,order = c(0,0,13))
a5=arima(return,order = c(8,0,4))
a6=arima(return,order = c(4,0,8))
a7=arima(return,order = c(8,0,8))
a8=arima(return,order = c(8,0,11))
#try different combination model

stargazer(a1,a2,a3,a4type = "text",out = 'C:/Users/Harry/Desktop/armafinal.txt', report = ('vcs*'))
stargazer(a5,a6,a7,a8,type = "text",out = 'C:/Users/Harry/Desktop/arma6.txt', report = ('vcs*'))
#out put of model result

print(a1$aic)
print(a2$aic)
print(a3$aic)
print(a4$aic)
print(a5$aic)
print(a6$aic)
print(a7$aic)
print(a8$aic)
#get AIC

aa1=Arima(return,order = c(7,0,4))
aa2=Arima(return,order = c(4,0,8))
aa3=Arima(return,order = c(0,0,11))
aa4=Arima(return,order = c(0,0,13))
aa5=Arima(return,order = c(8,0,4))
aa6=Arima(return,order = c(4,0,8))
aa7=Arima(return,order = c(8,0,8))
aa8=Arima(return,order = c(8,0,11))


print(aa1$bic)
print(aa2$bic)
print(aa3$bic)
print(aa4$bic)
print(aa5$bic)
print(aa6$bic)
print(aa7$bic)
print(aa8$bic)
#get BIC

#ARCH part

resd1=a1$residuals
#get residual
plot(resd1)
hist(resd1,breaks = 50)
# plot residual

Box.test(resd1,lag=(length(resd1)/4),type = 'Ljung-Box',fitdf = 0)
#Ljung-Box test for corelation between residuals

resd12<-resd1^2
#get square of residual
acf(resd1,lag.max = 20)
pacf(resd1,lag.max = 10)
#plot the ACF and PACF of residual
acf(resd12,lag.max = 20)
pacf(resd12,lag.max = 20)
##plot the ACF and PACF of residual square
Box.test(resd12,lag=(1508/4),type = 'Ljung-Box',fitdf = 0)
#Ljung-Box test for corelation between residual square


#Model 1 GARCH(1,1)
spec1<-ugarchspec(
  variance.model = list(model = 'fGARCH', garchOrder = c(1,1),submodel='GARCH',variance.targeting=FALSE),
  mean.model = list(armaOrder=c(7,4)),
  distribution.model='norm',
  start.pars = list(),
  fixed.pars = list()
)
mod1<-ugarchfit(spec = spec1,data = return,solver.control = list(trace=0))

#model 2 iGARCH
spec2<-ugarchspec(
  variance.model = list(model = 'iGARCH', garchOrder = c(1,1),submodel=NULL ,variance.targeting=FALSE),
  mean.model = list(armaOrder=c(7,4)),
  distribution.model='norm',
  start.pars = list(),
  fixed.pars = list()
)
mod2<-ugarchfit(spec = spec2,data = return,solver.control = list(trace=0))

#model 3 GARCH-M(1,1)
spec3<-ugarchspec(
  variance.model = list(model = 'fGARCH', garchOrder = c(1,1),submodel='GARCH',variance.targeting=FALSE),
  mean.model = list(armaOrder=c(7,4),archm=TRUE),
  distribution.model='norm',
  start.pars = list(),
  fixed.pars = list()
)
mod3<-ugarchfit(spec = spec3,data = return,solver.control = list(trace=0))

#model 4 TARCH-M(1,1)
spec4<-ugarchspec(
  variance.model = list(model = 'fGARCH', garchOrder = c(1,1),submodel='TGARCH',variance.targeting=FALSE),
  mean.model = list(armaOrder=c(7,4),archm=TRUE),
  distribution.model='norm',
  start.pars = list(),
  fixed.pars = list()
)
mod4<-ugarchfit(spec = spec4,data = return,solver.control = list(trace=0))

cof<-mod1@fit$matcoef
cof2<-mod2@fit$matcoef
cof3<-mod3@fit$matcoef
cof4<-mod4@fit$matcoef
stargazer(cof,cof2,cof3,cof4,type = 'text',out = 'C:/Users/Harry/Desktop/archout2.txt')
#get the coefficient of four models

TGARCH_M_STANDARD_RESIDUAL=mod4@fit$residuals/mod4@fit$sigma
#get the residual of model
TGARCH_M_STANDARD_RESIDUAL_SQR=(mod4@fit$residuals/mod4@fit$sigma)^2
#get the residual square of model
ACF=acf(TGARCH_M_STANDARD_RESIDUAL)
PACF=pacf(TGARCH_M_STANDARD_RESIDUAL)
plot(ACF,ci=0.975)
plot(PACF,ci=0.975)
#plot the ACF anf PACF of model
Box.test(TGARCH_M_STANDARD_RESIDUAL,lag =10,type = 'Ljung-Box',fitdf = 0)
Box.test(TGARCH_M_STANDARD_RESIDUAL_SQR,lag =10,type = 'Ljung-Box',fitdf = 0)
#Ljung-Box test for autocorelation between residuals and residual square
