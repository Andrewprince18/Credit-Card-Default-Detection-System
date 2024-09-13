mutinfo <- function(X,Y){
  ex_data <- data.frame(X,Y)
  data_tab <- table(ex_data)
  n_data <- nrow(ex_data)
  n_yvar <- ncol(data_tab) 
  n_xvar <- nrow(data_tab)
  
  sum_p <- matrix(NA,nrow = n_xvar ,ncol = n_yvar)
  for (i in 1:n_xvar){
    for (j in 1:n_yvar){
      pxy <- data_tab[i,j]/n_data
      px <- sum(data_tab[i,])/n_data
      py <- sum(data_tab[,j])/n_data
      
      if(pxy == 0 | is.na(pxy) | px == 0 | is.na(px) | py == 0 | is.na(py)){
        next } # 如果其中一個為 0 或 NA，則跳過此次迴圈
      
      sum_p[i,j] <- pxy*log((pxy)/(px*py))
    }
  }
  return(sum(sum_p, na.rm = TRUE))
}
