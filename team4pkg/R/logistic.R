#' Logistic Regression
#'
#'This function performs a logistic regression using the features in the data. 
#' @param X is an n x p data.matrix of features
#' @param Y is an n x 1 data.matrix of binary outcomes
#' @param start is the starting value
#' @param tol is the tolerance level (default is 1e-4)
#' @param maxit is the maximum iterations before ceasing maximization (default is 50)
#' @param verbose is the option to print iterated values (default is 1, 0 = no print)
#' @examples
#' \dontrun{
#'logistic(X,Y,tol=1e-5,maxit=10,verbose=0)
#'}
#' @export
  
logistic = function(X = NULL, Y = NULL, start = NULL, tol = 1e-4, maxit = 50, verbose = 1){
  
  p = dim(X)[2]
  start = numeric(p)
  
  iter = 0
  eps = Inf
  beta = start
  
  ## start the iteration
  while (iter < maxit & eps > tol){
    beta0 = beta
    
    d1 = deriv1.logistic(X = X, Y = Y, param = beta)
    d2 = deriv2.logistic(X = X, Y = Y, param = beta)
    beta = beta - solve(d2, d1)
    beta = beta[,1]
    
    eps  = sqrt(sum((beta - beta0)^2))
    
    iter = iter + 1
    if (iter == maxit){
      warning("Iteration limit reached without convergence")
    }
    
    if (verbose == 1){
      cat(sprintf("Iter: %d eps:%f\n",iter, eps))
    }
  }
  
  return(beta)
}
