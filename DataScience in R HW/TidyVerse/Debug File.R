GDP = NULL
for (i in unique(da$year)) {
  te = da %>% filter(year == i) %>% group_by(continent) %>% summarise(tol_GDP = sum(total_GDP)) %>% arrange(tol_GDP,continent)
  GDP = cbind(GDP,as.matrix(te))
}
