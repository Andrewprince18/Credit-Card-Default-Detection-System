#plot
roc(filter_data_loan_test[,'TARGET'],pre_respon,levels=c(0,1),direction="<", plot = T ,legacy.axes=T,
    percent=T,print.auc=T,main = "ROC Curve for test set", xlab = "False Positive Rate", ylab = "True Positive Rate"
    , auc.polygon = T, auc.polygon.col = 'gray',partial.auc=c(100,90)) #,partial.auc=c(100,90)


# 創建一個空的 dataframe 來存儲結果
result_df <- data.frame(threshold = numeric(0), precision = numeric(0), recall = numeric(0), f1 = numeric(0), pos_pred_value = numeric(0), accuracy = numeric(0))

# 對於每個切點
for (threshold in seq(0.01, 0.99, by = 0.01)) {
  # 將預測分數轉換為預測類別
  predicted <- factor(ifelse(pre_respon > threshold, 1, 0), levels = c("0", "1"))
  
  # 創建包含所有可能類別的空混淆矩陣
  full_confusion_matrix <- table(factor(filter_data_loan_test$TARGET, levels = c("0", "1")), predicted)
  
  # 創建混淆矩陣（只包含實際有出現的類別）
  confusion_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("0", "1"), c("0", "1")))
  confusion_matrix[rownames(full_confusion_matrix), colnames(full_confusion_matrix)] <- full_confusion_matrix
  
  # 計算相關指標
  precision  <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  pos_pred_value <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # 將計算結果添加到 dataframe 中
  result_df <- rbind(result_df, data.frame(threshold = threshold, precision = precision, recall = recall, f1 = f1, pos_pred_value = pos_pred_value, accuracy = accuracy))
}

# 查看整合後的 dataframe
result_df

#---
# 找到最高的 F1 值所對應的索引
max_f1_index <- which.max(result_df$f1)

# 提取最高 F1 值所對應的切點數值以及相應的數值
max_f1_threshold <- result_df$threshold[max_f1_index]
max_f1_value <- result_df$f1[max_f1_index]
max_precision <- result_df$precision[max_f1_index]
max_recall <- result_df$recall[max_f1_index]
max_pos_pred_value <- result_df$pos_pred_value[max_f1_index]
max_accuracy <- result_df$accuracy[max_f1_index]

# 輸出最高 F1 值所對應的切點以及相應的 precision、recall、pos_pred_value
cat("最高的 F1 值所對應的切點數值為:", max_f1_threshold, "\n")
cat("對應的 F1 值為:", max_f1_value, "\n")
cat("對應的 Precision 值為:", max_precision, "\n")
cat("對應的 Recall 值為:", max_recall, "\n")
cat("對應的 Pos Pred Value 值為:", max_pos_pred_value, "\n")
cat("對應的 Accuracy 值為:", max_accuracy, "\n")

cutdata = result_df[13,]
cutdata = melt(cutdata)
cutdata = data.frame(VAR = c("召回率", "精確率", "F1-score", "真陽性率", "準確率"), 
                     Value = c(0.24,0.47,0.32,0.47,0.77))
