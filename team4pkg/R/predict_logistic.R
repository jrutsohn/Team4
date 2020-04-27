#' Logistic Regression Prediction
#'
#' This function uses logistic regression to predict features.
#' @param X is a n x p matrix with p features
#' @param param is the parameter estimated by the logistic() function
#' @export
predict.logistic = function(X = NULL, param = NULL){
  if (dim(X)[2]!=length(param)){
    stop("Data and parameter dimensions do not match")
  }
  if (!is.vector(param)){
    if (dim(param)[2] != 1){
      stop("Input parameters need to be a vector")
    }
  }
  
  mu = 1 / (1 + exp(- X %*% param))
  return(mu[,1])
}

