y = c(1880,1920,1960,2000)
se = c("F","M")
re = NULL
for(i in y){
  for(j in se){
    a = filter(sam_da,year==i,sex==j) %>% arrange(desc(n))
    a[1:5,]
    re=rbind(re,as.matrix(a))
  }
}