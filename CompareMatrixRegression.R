# This function is to build a error matrix with 'mae','mse','rmse','mape'.
# Function required three arguments to be passed.
#     1) actuals (Actuals/Observations/trues) - This from input dataset observations
#     2) predictions (Preds) - predicted/forecated values
#     3) modlenme - Name of the model to name it on row

Compare_Regression_Models <- function(actuals,predictions,modlenme){
  if (exists("cmp_mtx_reg") == T)
  {
    rownm <- c(row.names(cmp_mtx_reg))
    rownm <- c(rownm,modlenme)
    err_matrix <- rbind(cmp_mtx_reg,c(regr.eval(trues = actuals, preds = predictions)))
    dimnames(err_matrix) <- list(c(rownm),c('mae','mse','rmse','mape'))
    assign("cmp_mtx_reg",err_matrix,envir=.GlobalEnv)
  }
  else
  {
    err_matrix <- matrix(data=regr.eval(trues = actuals, preds = predictions),ncol=4,byrow=F,dimnames=list(c(modlenme),c('mae','mse','rmse','mape')))
    assign("cmp_mtx_reg",err_matrix,envir=.GlobalEnv)
  }
  print(cmp_mtx_reg)
}
