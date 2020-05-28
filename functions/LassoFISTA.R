#' Computes Lasso solution with FISTA
#'
#' Computes the Lasso solution using the FISTA method of Beck and Teboulle (2014).
#' The objective function is given as the mean squared plus lambda times the L1-norm.
#' Simplified version.S
#' Last edited: 25 octobre 2019
#' 
#' @param y vector of the dependent variable, normalizing it is a good idea.
#' @param X Matrix of Covariates, quicker if normalized.
#' @param nopen Set of indices of variables that should not be penalized.
#' @param lambda Overall penalty parameter.
#' @param tol Stopping criteion: difference between the value of objective function at two iterations.
#' @param maxIter Maximal number of iterations for the algorithm.
#' @param trace if TRUE print algorithm info.
#' 
#' @return beta argmin of the function.
#' @return value Value of the function at argmin.
#' @return loss Value of mean squared error.
#' @return l1norm l1-norm of beta.
#' @return nbIter Number of iterations necessary for convergence.
#' @return convergence 1 if convergence, 0 if not.
#' 
#' @author Jeremy L'Hour


LassoFISTA <- function(y,X,nopen=NULL,lambda,
                       tol=1e-8,maxIter=1000,trace=F){
  
  ### set algo values
  eta = 1/max(2*eigen(t(X)%*%X)$values/nrow(X))
  theta = 1
  thetaO = theta
  beta = rep(0,ncol(X))
  v = beta
  cv = 1
  
  k = 0
  repeat{
    k = k+1
    
    thetaO = theta
    theta = (1+sqrt(1+4*thetaO^2))/2
    delta = (1-thetaO)/theta
    
    betaO = beta
    beta = prox(v - eta*ls_gradient(v,y,X), lambda*eta,nopen)
    
    v = (1-delta)*beta + delta*betaO
    
    # Show objective function value
    if(trace==T & k%%100 == 0){ print(paste("Objective Func. Value at iteration",k,":",lasso_obj(beta,y,X,lambda,nopen))) }
    
    # Break if diverges
    if(is.na(lasso_obj(beta,y,X,lambda,nopen) - lasso_obj(betaO,y,X,lambda,nopen))){
      cv = 0
      print("LassoFISTA did not converge")
      break
    } else if(sum(abs(lasso_obj(beta,y,X,lambda,nopen)-lasso_obj(betaO,y,X,lambda,nopen))) < tol || k > maxIter) break
    
  }
  
  if(k > maxIter){
    print("Max. number of iterations reach in Lasso minimization.")
    cv = 0
  } 
  
  return(list(beta        = as.vector(beta),
              value       = lasso_obj(beta,y,X,lambda,nopen),
              loss        = least_squares(beta,y,X),
              l1norm      = sum(abs(beta)),
              nbIter      = k,
              convergence = cv))
}


#################################
#################################
### Define auxiliary functions###
#################################
#################################

prox <- function(x,lambda,nopen){
  y = (abs(x)-lambda)*(abs(x)-lambda > 0) * sign(x)
  y[nopen] = x[nopen] # Do not penalize these variables
  return(y)
}

least_squares <- function(mu,y,X){
  X = as.matrix(X)
  return(mean((y - X%*%mu)^2))
}

ls_gradient <- function(mu,y,X){
  X = as.matrix(X)
  df = as.vector(-2*(t(y - X%*%mu)%*%X) / nrow(X))
  return(df)
}

lasso_obj <- function(beta,y,X,lambda,nopen){
  if(length(nopen)>0){
    f = least_squares(beta,y,X) + lambda*sum(abs(beta[-nopen]))
  } else {
    f = least_squares(beta,y,X) + lambda*sum(abs(beta))
  }
  return(f)
}