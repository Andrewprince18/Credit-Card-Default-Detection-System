#cv function
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]  #將資料分成K份，並生成的完成資料集n
  temp <- sample(n,datasize)  #把n打亂
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) #dataseq中隨機生成k個隨機有序資料列
  return(cvlist)
}