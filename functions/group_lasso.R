#' Group Lasso for categorical dependent variables
#'
#' Computes the Group Lasso for categorical dependent variables using FISTA.
#' The number of groups is equal to the number of explanatory variables (p).
#' Each group contains as many members as the number of modality for the dependent variable.
#' Created: 04/06/2020
#' 
#' @param y (n x t) matrix of dependent variable
#' @param X (n x p) matrix of covariates
#' @param lambda Overall penalty parameter
#' @param nopen Set of indices of variables that should not be penalized.
#' @param tol Stopping criteion: difference between the value of objective function at two iterations.
#' @param maxIter Maximal number of iterations for the algorithm.
#' @param trace if TRUE print algorithm info.
#' 
#' @return beta argmin of the function.
#' @return value Value of the function at argmin.
#' @return loss Value of mean squared error.
#' @return nbIter Number of iterations necessary for convergence.
#' @return convergence 1 if convergence, 0 if not.
#' 
#' @author Jérémy L'Hour

group_lasso <- function(X,y,lambda,nopen=NULL,tol=1e-8,maxIter=1000,trace=F){
  ### Add intercept and do not penalize
  X = cbind(X,rep(1,nrow(X)))
  nopen = c(nopen,ncol(X))
  
  ### set algo values
  eta = 1/max(2*eigen(t(X)%*%X)$values/nrow(X))
  theta = 1
  thetaO = theta
  beta = matrix(0,nrow = ncol(X),ncol = ncol(y))
  v = beta
  cv = 1
  
  t_start = Sys.time()
  k = 0
  repeat{
    k = k+1
    
    thetaO = theta
    theta = (1+sqrt(1+4*thetaO^2))/2
    delta = (1-thetaO)/theta
    
    betaO = beta
    beta = prox(v - eta*ls_grad_block(v,y,X),lambda*eta,nopen)
    
    v = (1-delta)*beta + delta*betaO
    
    # Show objective function value
    if(trace==T & k%%100 == 0){
      print(paste("Objective Func. Value at iteration",k,":",group_lasso_obj(beta,y,X,lambda,nopen)))
      }
    
    # Break if diverges
    if(is.na(group_lasso_obj(beta,y,X,lambda,nopen) - group_lasso_obj(betaO,y,X,lambda,nopen))){
      cv = 0
      print("Group-Lasso did not converge")
      break
    } else if(sum(abs(group_lasso_obj(beta,y,X,lambda,nopen)-group_lasso_obj(betaO,y,X,lambda,nopen))) < tol || k > maxIter){
      break
    } 
    
  }
  print(Sys.time()-t_start)
  
  if(k > maxIter){
    print("Max. number of iterations reach in Lasso minimization.")
    cv = 0
  } 
  
  return(list(beta        = beta,
              value       = group_lasso_obj(beta,y,X,lambda,nopen),
              loss        = least_squares(beta,y,X),
              nbIter      = k,
              convergence = cv))
}


#################################
#################################
### Define auxiliary functions###
#################################
#################################

ls_grad_block <- function(b,y,X) -2*t(X)%*%(y - X%*%b) / nrow(X)

least_squares <- function(b,y,X) mean((y - X%*%b)^2)

prox_vec <- function(x,lambda){
  if(sqrt(sum(x^2)) > lambda){
    new_x = x*(1-lambda/sqrt(sum(x^2)))
  } else {
    new_x = rep(0, length(x))
  }
  return(new_x)
}

prox <- function(b,lambda,nopen){
  new_b = apply(b,1,prox_vec,lambda=lambda)
  new_b = t(new_b)
  new_b[nopen,] = b[nopen,]
  return(new_b)
}


group_lasso_obj <- function(b,y,X,lambda,nopen){
  if(length(nopen)>0){
    f = least_squares(b,y,X) + lambda*sum(apply(b[-nopen,],1, function(x) sqrt(sum(x^2))))
  } else {
    f = least_squares(b,y,X) + lambda*sum(apply(b,1, function(x) sqrt(sum(x^2))))
  }
  return(f)
}