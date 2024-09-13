
#--------- Pearson cor -------------
compute_correlations <- function(data, target_var) {
  # 將目標變數與數值變數進行相關係數計算
  target_correlations <- cor(as.numeric(data[[target_var]]), data[, sapply(data, is.numeric)], use = "pairwise.complete.obs")
  
  # 創建一個資料框來存儲變數名和相關係數
  num_cor_df <- data.frame(VAR = names(data)[sapply(data, is.numeric)], cor_with_TARGET = as.vector(target_correlations))
  
  # 取絕對值並按大小排序
  sorted_num_cor_df <- num_cor_df[order(-abs(num_cor_df[,'cor_with_TARGET'])), ]
  
  # 返回排序后的結果
  return(sorted_num_cor_df)
}

compute_correlations(filter_data_loan, "TARGET")
compute_correlations(oversample_num_vars, "TARGET")
compute_correlations(downsample_num_vars, "TARGET")

#---------- mutual_information for category ------------
compute_mutual_information <- function(data, target_var) {
  factor_vars <- data[, sapply(data, is.factor)]  # 提取資料集中的因子變數
  nvar <- ncol(factor_vars)
  mi_results <- numeric(nvar)  # 創建一個空的向量來存儲 mutual information 的結果
  
  # 創建一個空的列表來存儲變數名稱和對應的 mutual information 值
  mi_list <- list()
  
  # 迴圈從第一個變數到第 nvar 個變數
  for (i in 1:nvar) {
    # 計算第 i 個變數與目標變數的 mutual information
    mi <- mutinfo(factor_vars[, i], data[[target_var]])
    # 將結果存儲到 mi_results 中的第 i 個位置
    mi_results[i] <- mi
    
    # 將變數名稱和對應的 mutual information 值添加到列表中
    mi_list[[i]] <- list(variable = colnames(factor_vars)[i], mutual_information = mi)
  }
  
  # 使用 order 函數將 mi_results 由大到小排序
  sorted_indices <- order(-mi_results)
  mi_results <- mi_results[sorted_indices]
  
  # 創建一個矩陣來存儲排序後的結果
  result_matrix <- matrix(NA, nrow = nvar, ncol = 2,
                          dimnames = list(NULL, c("Variable", "Mutual_Information")))
  # 將排序後的變數名稱與 mutual information 值填入矩陣中
  for (i in 1:nvar) {
    index <- sorted_indices[i]
    result_matrix[i, "Variable"] <- mi_list[[index]]$variable
    result_matrix[i, "Mutual_Information"] <- mi_list[[index]]$mutual_information
  }
  
  # 返回排序後的矩陣結果
  return(result_matrix)
}
# 使用範例:
compute_mutual_information(filter_data_loan, "TARGET")
compute_mutual_information(oversample_loan_data, "TARGET")
compute_mutual_information(downsample_loan_data, "TARGET")

#------ point-biserial for pvalue -------
calculate_cortest_summary <- function(data, response_var) {
  numvar <- data[, sapply(data, is.numeric)]
  cor_summary <- matrix(nrow=ncol(numvar), ncol=3)
  
  for (i in 1:ncol(numvar)) {
    cor_test <- cor.test(numvar[,i], as.numeric(data[[response_var]]))
    cor_summary[i,1] <- names(numvar)[i]
    cor_summary[i,2] <- round(cor_test$estimate, 6)
    cor_summary[i,3] <- round(cor_test$p.value, 6)
  }
  
  colnames(cor_summary) <- c("VAR", "CORR", "P-VAL")
  cor_summary <- as.data.frame(cor_summary)
  cor_summary[,2] <- as.numeric(cor_summary[,2])
  cor_summary[,3] <- as.numeric(cor_summary[,3])
  sorted_indices <- order(abs(cor_summary[, 2]), decreasing = TRUE)
  cor_summary <- cor_summary[sorted_indices,]
  return(cor_summary)
}

# cor_test_for_csv <- rbind(calculate_cortest_summary(num_vars, 'TARGET'),
#                    NA, calculate_cortest_summary(oversample_num_vars, 'TARGET'),
#                    NA, calculate_cortest_summary(downsample_num_vars, 'TARGET')
#                    )
cor_test_for_csv1 <- calculate_cortest_summary(num_vars, 'TARGET')
cor_test_for_csv2 <- calculate_cortest_summary(oversample_num_vars, 'TARGET')
cor_test_for_csv3 <- calculate_cortest_summary(downsample_num_vars, 'TARGET')
#write.csv(cor_test_for_csv,file = "pb_test_pval.csv", row.names = FALSE)

#把p值過小的改成"<0.001"
cor_test_for_csv1[,3] <- ifelse(cor_test_for_csv1[,3] < 0.001, "<0.001", cor_test_for_csv1[,3])
