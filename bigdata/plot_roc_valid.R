#plot logistic auc for validation set
plot_roc_valid=function(pos_data,neg_data,k,seed,model.fun,imbalancedata,yvar){
  #從CVgroup function 分出各k份的positive、negative data
  cvlist1 <- CVgroup(k,datasize = nrow(pos_data),seed)
  cvlist0 <- CVgroup(k,datasize = nrow(neg_data),seed+123)
  
  for(i in 1:k){
    
    test_fold<-rbind(pos_data[cvlist1[[i]],],neg_data[cvlist0[[i]],])
    train_fold<-rbind(pos_data[-cvlist1[[i]],],neg_data[-cvlist0[[i]],])
    
    #選擇train set y變數
    train.yvar<-train_fold[,yvar]
    #選擇test set y變數
    test.yvar<-test_fold[,yvar]
    
    #imbalanced data(套件caret)
    set.seed(seed+i*k)
    ifelse(imbalancedata=="no",traindata<-train_fold,F)
    ifelse(imbalancedata=="up",traindata<-upSample(x=train_fold,y=train.yvar),F)
    ifelse(imbalancedata=="down",traindata<-downSample(x=train_fold,y=train.yvar),F)
    traindata <- traindata[, !(names(traindata) == "Class"), drop = FALSE]
    
    #預測模型
    glm.fit<-glm(model.fun,data=traindata,family=binomial)
    pre_prob<-predict(glm.fit,newdata = test_fold)
    
    if(i == 1){
      roc(test_fold[,yvar],pre_prob,levels=c(0,1),direction="<", plot = T ,legacy.axes=T,
          percent=T,print.auc=T, print.auc.x = 90,print.auc.y=35,
          main = "ROC Curve for validation set", xlab = "False Positive Rate", ylab = "True Positive Rate")
    }else if (i==2){
      plot.roc(test_fold[,yvar],pre_prob,percent=T,col='blue',
               print.auc=T,add=T,print.auc.y=25 , print.auc.x = 90)
    }else if (i==3){
      plot.roc(test_fold[,yvar],pre_prob,percent=T,col='red',
               print.auc=T,add=T,print.auc.y=15, print.auc.x = 90)
    }else{
      plot.roc(test_fold[,yvar],pre_prob,percent=T,col='green',
               print.auc=T,add=T,print.auc.y=5, print.auc.x = 90)
    }
  }
  legend('bottomright',legend = c('k-fold = 1' ,'k-fold = 2' , 'k-fold = 3','k-fold = 4') ,
         col = c('black','blue','red','green'),lwd = 4)
}

# +FLAG_MOBIL
plot_roc_valid(target_1_data,target_0_data,4,123
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
          FLAG_DOCUMENT_21
        ,"no","TARGET")