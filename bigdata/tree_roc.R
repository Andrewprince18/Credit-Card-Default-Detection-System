tree_roc=function(pos_data,neg_data,k,seed,model.fun,imbalancedata,yvar){
  #從CVgroup function 分出各k份的positive、negative data
  cvlist1 <- CVgroup(k,datasize = nrow(pos_data),seed)
  cvlist0 <- CVgroup(k,datasize = nrow(neg_data),seed+123)
  
  roc.info<-list(0,0,0,0)
  
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
    pre.model<-rpart(model.fun,data=traindata)
    
    #算出預測機率
    pre_prob<-predict(pre.model,newdata = test_fold)
    
    #roc
    roc.info[[i]]<-roc(test_fold[,yvar],pre_prob[,2],levels = c(0, 1), direction = "<")
    roc.info[[i]]<-as.numeric(roc.info[[i]]$auc)
  }
  roc.mean<-mean(c(roc.info[[1]],roc.info[[2]],roc.info[[3]],roc.info[[4]]))
  roc.sd<-sd(c(roc.info[[1]],roc.info[[2]],roc.info[[3]],roc.info[[4]]))
  print(roc.mean)
  print(roc.sd)
}

#---feat select-----
tree_roc(target_1_data,target_0_data,4,123
      ,TARGET~OCCUPATION_TYPE+ORGANIZATION_TYPE+NAME_INCOME_TYPE+REGION_RATING_CLIENT_W_CITY+NAME_EDUCATION_TYPE+
        EXT_SOURCE_2+EXT_SOURCE_3+YEARS_BIRTH+YEARS_LAST_PHONE_CHANGE+YEARS_ID_PUBLISH
      ,"no","TARGET")

tree_roc(target_1_data,target_0_data,4,123
         ,TARGET~OCCUPATION_TYPE+ORGANIZATION_TYPE+NAME_INCOME_TYPE+REGION_RATING_CLIENT_W_CITY+NAME_EDUCATION_TYPE+
           EXT_SOURCE_2+EXT_SOURCE_3+YEARS_BIRTH+YEARS_LAST_PHONE_CHANGE+YEARS_ID_PUBLISH
         ,"up","TARGET")

tree_roc(target_1_data,target_0_data,4,123
         ,TARGET~OCCUPATION_TYPE+ORGANIZATION_TYPE+NAME_INCOME_TYPE+REGION_RATING_CLIENT_W_CITY+NAME_EDUCATION_TYPE+
           EXT_SOURCE_2+EXT_SOURCE_3+YEARS_BIRTH+YEARS_LAST_PHONE_CHANGE+YEARS_ID_PUBLISH
         ,"down","TARGET")
