#' Second Derivative
#'
#' This function calculates the Hessian matrix for the logistic regression function.
#' @param X is an n x p matrix of features
#' @param Y is an n x 1 vector of veatures
#' @param param is the parameter to be estimated by logistic()
#' @export
deriv2.logistic = function(X = NULL, Y = NULL, param = NULL){
  if (dim(X)[1] != length(Y)){
    stop("Input feature matrix dimension differs from outcome vector length")
  }
  
  mu = predict.logistic(X = X, param = param)
  
  ## obtain the weight matrix W
  W = diag(mu * (1-mu))
  
  deriv2 = - t(X) %*% W %*% X
  return(deriv2)
}

