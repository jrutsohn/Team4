#' First Derivative
#'
#' This function calculates the first derivative of the log likelihood function of the logistic regression GLM.
#' @param X is a n x p matrix with p features
#' @param Y is an n x 1 vector of binary outcomes
#' @param param is the parameter to be estimated in logistic() function
#' @export
deriv1.logistic = function(X = NULL, Y = NULL, param = NULL){
  if (dim(X)[1] != length(Y)){
    stop("Input feature matrix dimension differs from outcome vector length")
  }
  
  mu = predict.logistic(X = X, param = param)
  deriv1 = t(X) %*% (Y - mu)
  return(deriv1)
}

