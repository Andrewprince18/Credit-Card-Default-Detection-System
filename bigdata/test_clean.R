# test data
file_path <- "creditcard_test_true.csv"
test_data_loan_true <- read.csv(file_path, na.strings = c("", "NA"))


set.seed(123)
##missing value

filter_missingdata <- function(df, percentage) {
  # 計算每個變數中遺失值的百分比
  missing_percentage <- colMeans(is.na(df)) * 100
  # 找出超過 32% 遺失值的變數
  vars_to_keep <- names(missing_percentage)[missing_percentage <= percentage]
  # 從資料中刪除超過 32% 遺失值的變數
  df <- df[, vars_to_keep]
  return(df)
}

filter_data_loan_test<-filter_missingdata(test_data_loan_true,32)


#----------------------新增MISSING_RATIO----------------------
# 計算每筆資料中的遺失值數量
missing_count <- rowSums(is.na(filter_data_loan_test))
# 計算每筆資料中遺失值的比例
missing_ratio <- missing_count / ncol(filter_data_loan_test)
# 將遺失值比例新增為新的變數
filter_data_loan_test$MISSING_RATIO <- missing_ratio

#---------------------天數轉年數-------------------------
replace_days_with_years <- function(variable_name, data) {
  # 檢查給定變量名是否在數據框中存在
  if (!variable_name %in% names(data)) {
    print("變量名稱在數據框中未找到。")
    return(NULL)
  }
  
  # 提取變量值並進行轉換
  days_values <- data[[variable_name]]
  years_values <- round(abs(days_values) / 365.25)
  
  # 構造新變量名
  new_variable_name <- gsub("DAYS_", "YEARS_", variable_name)
  
  # 替換數據框中的變量名和結果
  data[[new_variable_name]] <- years_values
  data[[variable_name]] <- NULL  # 刪除原始變量列
  
  # 返回更新後的數據框
  return(data)
}

# 要替換的變量名列表
variables_to_replace <- c("DAYS_BIRTH", "DAYS_EMPLOYED", "DAYS_REGISTRATION",
                          "DAYS_ID_PUBLISH", "DAYS_LAST_PHONE_CHANGE")
# 一次性替換所有變量名
for (variable in variables_to_replace) {
  filter_data_loan_test <- replace_days_with_years(variable, filter_data_loan_test)
}

#---------------------num資料轉換int-------------------------
# 選擇所有以 "FLAG_DOCUMENT" 開頭的列
flag_doc_columns <- grep("^FLAG_DOCUMENT", names(filter_data_loan_test), value = TRUE)

# 選擇所有以 "AMT_REQ_CREDIT_BUREAU" 開頭的列
amt_req_columns <- grep("^AMT_REQ_CREDIT_BUREAU", names(filter_data_loan_test), value = TRUE)

# 其他指定的數值變量也需轉換為整數型，添加這些變量名
additional_columns <- c("CNT_FAM_MEMBERS", "YEARS_BIRTH", "YEARS_EMPLOYED", "YEARS_REGISTRATION", 
                        "YEARS_ID_PUBLISH", "YEARS_LAST_PHONE_CHANGE", "OBS_30_CNT_SOCIAL_CIRCLE", 
                        "DEF_30_CNT_SOCIAL_CIRCLE", "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE")

# 合併列名並進行轉換
all_columns_to_convert <- c(flag_doc_columns , amt_req_columns, additional_columns)
filter_data_loan_test[, all_columns_to_convert] <- lapply(filter_data_loan_test[, all_columns_to_convert], as.integer)

#---------------------類別資料修改遺失值---------------------
# M = 1, F = 0
## XNA 改成 others
filter_data_loan_test$CODE_GENDER[filter_data_loan_test$CODE_GENDER == "XNA"] <- "others"

# 將 'ORGANIZATION_TYPE' 中的 'XNA' 替換為 'Pensioner'
filter_data_loan_test$ORGANIZATION_TYPE[filter_data_loan_test$ORGANIZATION_TYPE == "XNA"] <- "Pensioner"
# 将 ORGANIZATION_TYPE 中的特定字串進行替換
filter_data_loan_test$ORGANIZATION_TYPE <- gsub("Business Entity Type [1-3]", "Business Entity", filter_data_loan_test$ORGANIZATION_TYPE)
filter_data_loan_test$ORGANIZATION_TYPE <- gsub("Industry: type [1-9]", "Industry", filter_data_loan_test$ORGANIZATION_TYPE)
filter_data_loan_test$ORGANIZATION_TYPE <- gsub("Trade: type [1-7]", "Trade", filter_data_loan_test$ORGANIZATION_TYPE)
filter_data_loan_test$ORGANIZATION_TYPE <- gsub("Transport: type [1-4]", "Transport", filter_data_loan_test$ORGANIZATION_TYPE)
filter_data_loan_test$ORGANIZATION_TYPE <- gsub("^Industry[0-3]$", "Industry", filter_data_loan_test$ORGANIZATION_TYPE)

# 將 "OCCUPATION_TYPE" 變數的遺失值設定為 "others"
filter_data_loan_test$OCCUPATION_TYPE[is.na(filter_data_loan_test$OCCUPATION_TYPE)] <- "others"

# 將 "NAME_TYPE_SUITE" 變數的遺失值設定為 "Non_collected"
filter_data_loan_test$NAME_TYPE_SUITE <- ifelse(is.na(filter_data_loan_test$NAME_TYPE_SUITE), "Non_collected", filter_data_loan_test$NAME_TYPE_SUITE) # Other_A、Other_B、Non_collected(na)

colSums(is.na(filter_data_loan_test))
#------------數值型遺失值處理--------------------
# 指定需要填补的数值型变量列表
numeric_vars <- c("AMT_ANNUITY", "AMT_GOODS_PRICE", "EXT_SOURCE_2")

# 使用 sapply() 函数对每个数值型变量进行缺失值填补（以平均值填补）
for (var in numeric_vars) {
  filter_data_loan_test[[var]][is.na(filter_data_loan_test[[var]])] <- mean(filter_data_loan_test[[var]], na.rm = TRUE)
}

#int型數值處理
# 計算 CNT_FAM_MEMBERS 列的眾數
mode_cnt_fam_members <- as.integer(names(which.max(table(filter_data_loan_test$CNT_FAM_MEMBERS, useNA = "no"))))
# 以眾數取代遺失的數值
filter_data_loan_test$CNT_FAM_MEMBERS[is.na(filter_data_loan_test$CNT_FAM_MEMBERS)] <- mode_cnt_fam_members

mode_YEARS_LAST_PHONE_CHANGE <- as.integer(names(which.max(table(filter_data_loan_test$YEARS_LAST_PHONE_CHANGE, useNA = "no"))))
# 以眾數取代遺失的數值
filter_data_loan_test$YEARS_LAST_PHONE_CHANGE[is.na(filter_data_loan_test$YEARS_LAST_PHONE_CHANGE)] <- mode_YEARS_LAST_PHONE_CHANGE

# 刪除含有遺失值的資料行
# 指定需要檢查的變數
vars_to_check <- c("OBS_30_CNT_SOCIAL_CIRCLE", "DEF_30_CNT_SOCIAL_CIRCLE", 
                   "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE")

# 利用 dplyr 的 filter 函數和 complete.cases() 來刪除含遺失值的資料行
filter_data_loan_test <- filter_data_loan_test %>% 
  filter(complete.cases(.[vars_to_check]))

#-------------- mice imputation ------------------
#將num型變數的遺失值用多重插補法填補
library(mice)

AMT.data <- mice(filter_data_loan_test[,c('AMT_REQ_CREDIT_BUREAU_YEAR', 'AMT_REQ_CREDIT_BUREAU_QRT', 'AMT_REQ_CREDIT_BUREAU_MON', 
                                     'AMT_REQ_CREDIT_BUREAU_WEEK', 'AMT_REQ_CREDIT_BUREAU_DAY','AMT_REQ_CREDIT_BUREAU_HOUR')],
                 m=5, maxit=5, meth='pmm', seed=88,print=FALSE)

filter_data_loan_test[,c('AMT_REQ_CREDIT_BUREAU_HOUR', 'AMT_REQ_CREDIT_BUREAU_DAY', 'AMT_REQ_CREDIT_BUREAU_WEEK',
                    'AMT_REQ_CREDIT_BUREAU_MON', 'AMT_REQ_CREDIT_BUREAU_QRT', 'AMT_REQ_CREDIT_BUREAU_YEAR')] <- complete(AMT.data,1)


# 计算要生成缺失值的索引位置
num_missing <- round(0.1 * length(filter_data_loan_test$YEARS_BIRTH))
missing_index <- sample(length(filter_data_loan_test$YEARS_BIRTH), num_missing)

# 创建带有缺失值的新向量
YB_mis <- filter_data_loan_test$YEARS_BIRTH  # 复制原始数据
YB_mis[missing_index] <- NA  # 将选定的索引位置的值替换为 NA，表示缺失值
filter_data_loan_test$YEARS_BIRTH <- YB_mis  # 将新向量替换为原始数据框中的 YEARS_BIRTH 列
# summary(filter_data_loan_test$YEARS_BIRTH)


# 进行多重插补
imp.data <- mice(filter_data_loan_test[c('EXT_SOURCE_3', 'YEARS_BIRTH')],
                 m = 5, maxit = 5, seed = 87, print = FALSE, meth = 'pmm')

filter_data_loan_test[c('EXT_SOURCE_3', 'YEARS_BIRTH')] <- complete(imp.data, 4)

#--------------------------------------
# 有level的轉factor
filter_data_loan_test$NAME_EDUCATION_TYPE <- factor(filter_data_loan_test$NAME_EDUCATION_TYPE, levels = c("Lower secondary", "Secondary / secondary special", "Incomplete higher", "Higher education", "Academic degree"), labels = c(0, 1, 2, 3, 4))
filter_data_loan_test$NAME_HOUSING_TYPE <- factor(filter_data_loan_test$NAME_HOUSING_TYPE, levels = c("With parents", "House / apartment", "Municipal apartment", "Rented apartment", "Office apartment", "Co-op apartment"), labels = c(0, 1, 2, 3, 4, 5))

# 沒有level要轉換為因子的變數名稱列表
variables_to_convert <- c('TARGET',"FLAG_OWN_CAR", "FLAG_OWN_REALTY","FLAG_MOBIL",
                          "FLAG_EMP_PHONE", "FLAG_WORK_PHONE", "FLAG_CONT_MOBILE",
                          "FLAG_PHONE", "FLAG_EMAIL", "REGION_RATING_CLIENT",
                          "REGION_RATING_CLIENT_W_CITY", "WEEKDAY_APPR_PROCESS_START",
                          "REG_REGION_NOT_LIVE_REGION", "REG_REGION_NOT_WORK_REGION",
                          "LIVE_REGION_NOT_WORK_REGION", "REG_CITY_NOT_LIVE_CITY",
                          "REG_CITY_NOT_WORK_CITY", "LIVE_CITY_NOT_WORK_CITY"
                          ,"NAME_INCOME_TYPE","NAME_FAMILY_STATUS","OCCUPATION_TYPE"
                          ,"CODE_GENDER","NAME_TYPE_SUITE"
                          ,"ORGANIZATION_TYPE","NAME_CONTRACT_TYPE")
table(filter_data_loan_test$NAME_INCOME_TYPE)

# 使用 lapply 將變數轉換為因子
filter_data_loan_test[variables_to_convert] <- lapply(filter_data_loan_test[variables_to_convert], as.factor)


#----------------------新增SUM_FLAG_DOCUMENT----------------------
merge_data_doc <- function(df) {
  # 計算所有以 FLAG_DOCUMENT 開頭的變數的和
  df$SUM_FLAG_DOCUMENT <- rowSums(df[, grepl("FLAG_DOCUMENT", names(df))], na.rm = TRUE)
  # 返回修改後的數據框
  return(df)
}
filter_data_loan_test <- merge_data_doc(filter_data_loan_test)
#將 FLAG_DOCUMENT_2 到 FLAG_DOCUMENT_21 列轉換為 factor
for (i in 2:21) {
  filter_data_loan_test[[paste0("FLAG_DOCUMENT_", i)]] <- as.factor(filter_data_loan_test[[paste0("FLAG_DOCUMENT_", i)]])
}

#----------------------新增SUM_AMT_REQ_CREDIT_BUREAU----------------------
add_sum_amt_req_credit_bureau <- function(df) {
  # 計算所有以 AMT_REQ_CREDIT_BUREAU 開頭的變數的和
  df$SUM_AMT_REQ_CREDIT_BUREAU <- rowSums(df[, grepl("^AMT_REQ_CREDIT_BUREAU", names(df))], na.rm = FALSE)
  # 返回修改後的數據框
  return(df)
}
filter_data_loan_test <- add_sum_amt_req_credit_bureau(filter_data_loan_test)

#sum變數做整數型態轉換
filter_data_loan_test$SUM_FLAG_DOCUMENT <- as.integer(filter_data_loan_test$SUM_FLAG_DOCUMENT)
filter_data_loan_test$SUM_AMT_REQ_CREDIT_BUREAU <- as.integer(filter_data_loan_test$SUM_AMT_REQ_CREDIT_BUREAU)

#刪除少數數值型遺失資料
filter_data_loan_test <- subset(filter_data_loan_test, !is.na(AMT_ANNUITY) & !is.na(AMT_GOODS_PRICE))













