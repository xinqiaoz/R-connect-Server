FVX = read.csv("^FVX.csv") 

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

tbl_FCRDX  = FCRDX %>% as_tbl_time(index = Date)
