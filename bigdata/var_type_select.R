
select_variables <- function(data, type = c("num", "factor"), exclude = NULL) {
  # 確保 type 參數的合法性
  type <- match.arg(type)
  
  if (!is.null(exclude)) {
    exclude <- unlist(exclude)
  }
  
  if (type == "num") {
    # 篩選數值型變數
    selected_vars <- data[, sapply(data, is.numeric)]
  } else if (type == "factor") {
    # 篩選類別型變數
    selected_vars <- Filter(is.factor, data)
  }
  
  # 如果有指定要排除的變數，則從篩選結果中排除
  if (!is.null(exclude)) {
    selected_vars <- selected_vars[, !(colnames(selected_vars) %in% exclude)]
  }
  
  # 返回篩選結果
  return(selected_vars)
}

#-------------------------
# 使用函數選擇數值型變數，並排除 "ID" 和 "Date" 變數
num_vars <- select_variables(filter_data_loan, type = "num", exclude = 'SK_ID_CURR')
# 使用函數選擇類別型變數，並排除 "TARGET" 變數
factor_vars <- select_variables(filter_data_loan, type = "factor", exclude = "TARGET")

#--------------------------
target_1_data <- subset(filter_data_loan, TARGET == 1)
target_0_data <- subset(filter_data_loan, TARGET == 0)

#-----subsampling data-----------
set.seed(123)
oversample_loan_data <- upSample(x=filter_data_loan,y=filter_data_loan$TARGET)
downsample_loan_data <- downSample(x=filter_data_loan,y=filter_data_loan$TARGET)

# for corr test (package:dplyr)
num_vars <- cbind(num_vars,filter_data_loan$TARGET)
names(num_vars)[ncol(num_vars)] <- "TARGET"
oversample_num_vars <- select_variables(oversample_loan_data, type = "num", exclude = 'SK_ID_CURR')
oversample_num_vars <- cbind(oversample_num_vars,oversample_loan_data$TARGET)
names(oversample_num_vars)[ncol(oversample_num_vars)] <- "TARGET"
downsample_num_vars <- select_variables(downsample_loan_data, type = "num", exclude = 'SK_ID_CURR')
downsample_num_vars <- cbind(downsample_num_vars,downsample_loan_data$TARGET)
names(downsample_num_vars)[ncol(downsample_num_vars)] <- "TARGET"

