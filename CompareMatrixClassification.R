# This function is to build a compare matrix with train and test accuracy, recall, precision and F Score.
# Function required three arguments to be passed.
#     1) Model Name -
#     2) Training features
#     3) Training Class variable
#     4) Test features
#     5) Test class variable
#     3) modlenme - Name of the model to name the row

Compare_Classification_Models <- function(model_c,train_ind,train_class,test_ind,test_class,modlenme){
  if (exists("cmp_mtx_cls") == T)
  {
    rownm <- c(row.names(cmp_mtx_cls))
    rownm <- c(rownm,modlenme)
    
    compare_Train <- table(predict(model_c,train_ind),train_class)
    compare_Test <- table(predict(model_c,test_ind),test_class)
    
    Train_Acu <- sum(diag(compare_Train))/sum(compare_Train)
    Train_Recall <- compare_Train[2,2]/sum(compare_Train[2,])
    Train_pre <- compare_Train[2,2]/sum(compare_Train[,2])
    
    Test_accu <- sum(diag(compare_Test))/sum(compare_Test)
    Test_Recall <- compare_Test[2,2]/sum(compare_Test[2,])
    Test_pre <- compare_Test[2,2]/sum(compare_Test[,2])
    
    F_Train = (2 * Train_pre * Train_Recall)/(Train_Recall + Train_pre)
    F_Test = (2 * Test_pre * Test_Recall)/(Test_Recall + Test_pre)
    
    
    err_matrix <- rbind(cmp_mtx_cls,c(Train_Acu,Test_accu,Train_Recall,Test_Recall,Train_pre,Test_pre,F_Train,F_Test))
    dimnames(err_matrix) <- list(c(rownm),c('Train Acc','Test Acc','Train Recall','Test Recall','Train Pre','Test Pre','Train F','Test F'))
    assign("cmp_mtx_cls",err_matrix,envir=.GlobalEnv)
  }
  else
  {
    compare_Train <- table(predict(model_c,train_ind),train_class)
    compare_Test <- table(predict(model_c,test_ind),test_class)
    
    Train_Acu <- sum(diag(compare_Train))/sum(compare_Train)
    Train_Recall <- compare_Train[2,2]/sum(compare_Train[2,])
    Train_pre <- compare_Train[2,2]/sum(compare_Train[,2])
    
    Test_accu <- sum(diag(compare_Test))/sum(compare_Test)
    Test_Recall <- compare_Test[2,2]/sum(compare_Test[2,])
    Test_pre <- compare_Test[2,2]/sum(compare_Test[,2])
    
    F_Train = (2 * Train_pre * Train_Recall)/(Train_Recall + Train_pre)
    F_Test = (2 * Test_pre * Test_Recall)/(Test_Recall + Test_pre)
    
    err_matrix <- matrix(data=c(Train_Acu,Test_accu,Train_Recall,Test_Recall,Train_pre,Test_pre,F_Train,F_Test),ncol=8,byrow=F,dimnames=list(c(modlenme),c('Train Acc','Test Acc','Train Recall','Test Recall','Train Pre','Test Pre','Train F','Test F')))
    assign("cmp_mtx_cls",err_matrix,envir=.GlobalEnv)
  }
  print(cmp_mtx_cls)
}
