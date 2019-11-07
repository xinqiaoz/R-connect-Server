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
library(ggplot2)
ggplot()+geom_line(aes(y = quantile(data$Adj.Close,seq(0,1,0.01)),x = quantile(data$PX_LAST,seq(0,1,0.01))))+
  geom_abline(slope = 1,intercept = 0)
data = mutate(data,log.return = c(1,diff(log(data$Adj.Close))*100))
data = mutate(data,log.return.index = c(1,diff(log(data$PX_LAST))*100))
hist(data$log.return,breaks = 100,probability = T )
plot(x = data$Date,y = data$Adj.Close,type = "l")
plot(x = data$Date,y = data$PX_LAST,type = "l")
