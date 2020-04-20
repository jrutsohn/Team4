## current estimate of the expectations pi (mu)
## input X is a n-by-p matrix with p features
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



## First derivative
## input X is a n-by-p matrix with p features, Y is a length-n vector of 0-1
deriv1.logistic = function(X = NULL, Y = NULL, param = NULL){
  if (dim(X)[1] != length(Y)){
    stop("Input feature matrix dimension differs from outcome vector length")
  }
  
  mu = predict.logistic(X = X, param = param)
  deriv1 = t(X) %*% (Y - mu)
  return(deriv1)
}


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
