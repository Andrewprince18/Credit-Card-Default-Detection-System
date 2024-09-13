log_numval=function(pos_data,neg_data,k,seed,model.fun,imbalancedata,yvar){
  
  #從CVgroup function 分出各k份的positive、negative data
  cvlist1 <- CVgroup(k,datasize = nrow(pos_data),seed)
  cvlist0 <- CVgroup(k,datasize = nrow(neg_data),seed+123)
  
  #切點看要切成幾份
  seq1<-seq(0.01,0.99,by=0.01)
  matrix1<-matrix(seq1,nrow = k+1, ncol = length(seq1),byrow = TRUE)
  index1<-as.data.frame(matrix1)#變成矩陣指標
  index.val<-list(index1,index1,index1,index1)
  
  for(i in 1:k){
    
    test_fold<-rbind(pos_data[cvlist1[[i]],],neg_data[cvlist0[[i]],])
    train_fold<-rbind(pos_data[-cvlist1[[i]],],neg_data[-cvlist0[[i]],])
    
    #選擇y變數
    ifelse(yvar=="TARGET",train.yvar<-train_fold$TARGET,F)
    
    #imbalanced data(套件caret)
    set.seed(seed+i*k)
    ifelse(imbalancedata=="no",traindata<-train_fold,F)
    ifelse(imbalancedata=="up",traindata<-upSample(x=train_fold,y=train.yvar),F)
    ifelse(imbalancedata=="down",traindata<-downSample(x=train_fold,y=train.yvar),F)
    
    #預測模型
    pre.model=glm(model.fun,data=traindata,family=binomial)
    
    #算出預測機率
    pre_prob<-predict(pre.model,newdata = test_fold,type = "response")
    
    for(j in 1:99){
      pre_val<-ifelse(pre_prob>seq1[j],1,0)
      
      ifelse(yvar=="TARGET",test.yvar<-test_fold$TARGET,F)
      
      u <- union(pre_val,test.yvar)
      table.confu <- table(factor(pre_val,u),factor(test.yvar,u))
      
      outcome1<-confusionMatrix(table.confu,positive = "1")
      outcome1<-as.list(outcome1$byClass)
      
      index.val[[1]][i+1,j]<-outcome1$Sensitivity
      index.val[[2]][i+1,j]<-outcome1$Specificity
      index.val[[3]][i+1,j]<-outcome1$`Pos Pred Value`
      index.val[[4]][i+1,j]<-outcome1$F1
    }
    #ifelse(i==4,print(index.val),F)
  }
  
  index2<-as.data.frame(matrix(seq1,nrow = 3, ncol = length(seq1),byrow = TRUE))
  index.meansd<-list(index2,index2,index2,index2)
  
  for (i in 1:99){
    for(l in 1:4){
      index.meansd[[l]][2,i]<-mean(index.val[[l]][2:k,i])
      index.meansd[[l]][3,i]<-sd(index.val[[l]][2:k,i])
    }
  }
  
  allval_mean<-rbind(seq1,index.meansd[[1]][2,],index.meansd[[2]][2,],index.meansd[[3]][2,],index.meansd[[4]][2,])
  allval_sd<-rbind(seq1,index.meansd[[1]][3,],index.meansd[[2]][3,],index.meansd[[3]][3,],index.meansd[[4]][3,])
  rownames(allval_mean)<-c("thresholds","Sensitivity","Specificity","`Pos Pred Value`","F1")
  rownames(allval_sd)<-c("thresholds","Sensitivity","Specificity","`Pos Pred Value`","F1")
  
  output<-list(allval_mean,allval_sd)
  print(output)
}

#Sensitivity
#Specificity
#`Pos Pred Value`
#`Neg Pred Value`
#Precision
#Recall
#F1
#Prevalence
#`Detection Rate`
#`Detection Prevalence`
#`Balanced Accuracy`

#可得所有切點下全部fold計算的數值
log_numval(target_1_data,target_0_data,4,123
           ,TARGET~OCCUPATION_TYPE+NAME_INCOME_TYPE+REGION_RATING_CLIENT_W_CITY+NAME_EDUCATION_TYPE+ORGANIZATION_TYPE
           +CNT_CHILDREN+AMT_INCOME_TOTAL+AMT_CREDIT+AMT_ANNUITY+AMT_GOODS_PRICE+REGION_POPULATION_RELATIVE+CNT_FAM_MEMBERS+
             HOUR_APPR_PROCESS_START+EXT_SOURCE_2+EXT_SOURCE_3+OBS_30_CNT_SOCIAL_CIRCLE+DEF_30_CNT_SOCIAL_CIRCLE+
             OBS_60_CNT_SOCIAL_CIRCLE+DEF_60_CNT_SOCIAL_CIRCLE+AMT_REQ_CREDIT_BUREAU_HOUR+AMT_REQ_CREDIT_BUREAU_DAY+
             AMT_REQ_CREDIT_BUREAU_WEEK+AMT_REQ_CREDIT_BUREAU_MON+AMT_REQ_CREDIT_BUREAU_QRT+AMT_REQ_CREDIT_BUREAU_YEAR+
             MISSING_RATIO+YEARS_BIRTH+YEARS_EMPLOYED+YEARS_REGISTRATION+YEARS_ID_PUBLISH+YEARS_LAST_PHONE_CHANGE+SUM_FLAG_DOCUMENT+
             SUM_AMT_REQ_CREDIT_BUREAU+NAME_CONTRACT_TYPE+CODE_GENDER+FLAG_OWN_CAR+FLAG_OWN_REALTY+NAME_TYPE_SUITE+
             NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+FLAG_EMP_PHONE+FLAG_WORK_PHONE+FLAG_CONT_MOBILE+FLAG_PHONE
           +FLAG_EMAIL+OCCUPATION_TYPE+REGION_RATING_CLIENT+WEEKDAY_APPR_PROCESS_START+
             REG_REGION_NOT_LIVE_REGION+REG_REGION_NOT_WORK_REGION+LIVE_REGION_NOT_WORK_REGION+REG_CITY_NOT_LIVE_CITY+REG_CITY_NOT_WORK_CITY+
             LIVE_CITY_NOT_WORK_CITY+FLAG_DOCUMENT_2+FLAG_DOCUMENT_3+FLAG_DOCUMENT_4+FLAG_DOCUMENT_5+FLAG_DOCUMENT_6+
             FLAG_DOCUMENT_7+FLAG_DOCUMENT_8+FLAG_DOCUMENT_9+FLAG_DOCUMENT_10+FLAG_DOCUMENT_11+FLAG_DOCUMENT_12+FLAG_DOCUMENT_13+
             FLAG_DOCUMENT_14+FLAG_DOCUMENT_15+FLAG_DOCUMENT_16+FLAG_DOCUMENT_17+FLAG_DOCUMENT_18+FLAG_DOCUMENT_19+FLAG_DOCUMENT_20+
             FLAG_DOCUMENT_21,"no","TARGET")
