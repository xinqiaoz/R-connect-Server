setwd("D:/R-connect-Server/DataScience in R/Project/data preperation and EDA")
WV_RD <- readRDS("survey_data.rds")



library(tidyverse)
library(magrittr)
da = WV_RD
da %<>% select(V1:V250)
# test %>% apply(MARGIN = 2,function(x){unique(x)})

# all negative value represents NA

da = data.frame(apply(da,MARGIN = 2,function(x){x = ifelse(x<0, NA, x)}))

# Calculate the NA value of each variables
NA_NUM = data.frame(da %>% apply(MARGIN = 2,function(x){round(sum(is.na(x))/length(x),2)}))
NA_NUM["name"] = rownames(NA_NUM)
colnames(NA_NUM) = c("Prop","name")

# The distribution of NA of each variables
hist(NA_NUM$Prop,breaks = 20)

# most of the Variables miss less than 10%

# delete variables with nore than 10% NA
NA_NUM %<>% filter(Prop<=0.1)

da %<>% select(NA_NUM[,2])

# V1,V2A,V3 are code for questionaire
da %<>% select(-V1,-V2A,-V3)

#delete NA observations
da = na.omit(da)

cor = cor(da[,2:215])
# warning message say some variables have 0 standard diviation which means these variables have only one value, so delete it

# calculate the unique value of each variables.
ui = data.frame(da %>% apply(MARGIN = 2,function(x){unique(x)%>%length}))

da %<>% select(-V125_16,-V125_17)

cor = cor(da[,2:213])
cor_c = reshape2::melt(cor)
hist(cor_c$value,breaks = 1000)
car::qqPlot(cor_c$value,id=F)

# according to the correlation matrix, most of the variables are not correlated

## Cross Comparison
da %>% filter(V2==51) %>% with(table(V4,V5))

## Proportion calculation

col = function(x){
  xf = factor(x)
  xr = tapply(x, xf, length)/length(x)
  return(xr)
}


da %>% filter(V2 == 51) %>% with(col(V4))

#Factor Analysis

# Draw Samples from the data

sa = sample(18445,replace = F,size = 1000)
das = da[sa,]
dasa = da[sa,]

# transfer binary variables into factors:
ui["name"] = rownames(ui)
colnames(ui) = c("Uni_value","name")
cat = ui$name[which(ui$Uni_value==2)]
for(i in cat){
  das[i] = as.factor(as.matrix(das[i]))
}

# Calculate Correlation matrix
library(polycor)
cormat = hetcor(das[,2:213])$cor
cormat_c = reshape2::melt(cormat)
hist(cormat_c$value,breaks = 1000)
car::qqPlot(cormat_c$value,id=F)

# calculate Factor
library(psych)
wls.fa = fa(r = cormat,nfactors = 40,rotate = "varimax", fm = "wls")
ols.fa = fa(r = cormat,nfactors = 20,rotate = "varimax", fm = "ols") 
gls.fa = fa(r = cormat,nfactors = 20,rotate = "varimax", fm = "gls")
pa.fa = fa(r = cormat,nfactors = 20,rotate = "varimax", fm = "pa")
mle.fa = fa(r = cormat,nfactors = 20,rotate = "varimax", fm = "ml") 


# Calculate the principal components
lo = matrix(nrow = 212,ncol = 20)
va = as.matrix(dasa[,2:213])
for(i in 1 : 212){
  for(j in 1:20){
    lo[i,j] = mle.fa$loadings[i,j]
  }
}

dasa = data.frame(cbind(dasa[,1],va %*% lo))



### Cluster Analysis

library(corrplot)
cor = cor(test)
corrplot(cor, method = "shade",addCoef.col = T,addCoefasPercent = T)

# Try to use k-means clustering analysis, we have to decide the k value or the number of clusters
# We can decide by the plot of SSE, if k is smaller than the real number of clusters, as k increase, the SSE will decrease significantly, 
# but once k exceed the real number of clusters, the decrease of SSE will be smaller, so the plot of SSE will contains a obvious turning point.

library(cluster)
m=10 # number of clusters
n=10  # number of repeats experiments for each number of clusters
dasc = dasa[-1]
tot = matrix(nrow = m,ncol  = n)
f = matrix(nrow = m,ncol  = n)
for(i in 1:m){
  for(j in 1:n){
    cl = kmeans(x = dasc,centers = i)
    f[i,j] = cl$betweenss/cl$tot.withinss
    tot[i,j] = cl$tot.withinss
  }
}
tot %<>% apply(MARGIN = 1,mean)
f %<>% apply(MARGIN = 1,mean)
plot(tot,type = "l")
plot(f,type = "l")

# choose number of clusters 3
best = NULL
toot = 2.146540e+12
for(i in 1:100){
  cl = kmeans(dasc,4)
  if (cl$tot.withinss<toot) {
    toot = cl$tot.withinss
    best = cl$cluster
  }
}
toot

das["cluster"]=best
cl_for_each_country = das %>% group_by(V2) %>% summarise(num_of_c1 = sum(cluster==1),num_of_c2 = sum(cluster==2),num_of_c3 = sum(cluster==3))
cl_for_each_country %>% arrange(desc(num_of_c1)) %>% head
cl_for_each_country %>% arrange(desc(num_of_c2)) %>% head
cl_for_each_country %>% arrange(desc(num_of_c3)) %>% head
