# This function is to build a error matrix with 'mae','mse','rmse','mape'.
# Function required three arguments to be passed.
#     1) actuals (Actuals/Observations/trues) - This from input dataset observations
#     2) predictions (Preds) - predicted/forecated values
#     3) modlenme - Name of the model to name it on row

build_err_matrix <- function(actuals,predictions,modlenme){
  if (exists("err_mtx") == T) 
  {
    rownm <- c(row.names(err_mtx))
    rownm <- c(rownm,modlenme)
    err_matrix <- rbind(err_mtx,c(regr.eval(trues = actuals, preds = predictions)))
    dimnames(err_matrix) <- list(c(rownm),c('mae','mse','rmse','mape'))
    assign("err_mtx",err_matrix,envir=.GlobalEnv)
  }
  else
  {
    err_matrix <- matrix(data=regr.eval(trues = actuals, preds = predictions),ncol=4,byrow=F,dimnames=list(c(modlenme),c("a","b","c","d")))
    assign("err_mtx",err_matrix,envir=.GlobalEnv)
  }
  print(err_mtx)
}