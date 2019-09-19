for(i in 1:length(names(heights))){
  num_of_uniquevalue=length(unique(heights[,i]))
  Na = sum(is.na(heights[,i]))
  info = cbind(info,matrix(c(num_of_uniquevalue,Na),ncol = 1))
}