eps = sweep(treat_residuals,MARGIN=1,dbs_reg$residuals,`*`)
cluster_var = ID_menage 
df_adj = ncol(X_1) + length(S_hat) + 1


#' @param eps n x t matrix of residuals with row equal to X_{i,t} vareps_{i,t}
#' @param cluster_var variable that identifies the cluster, dimension n
#' @param dj_adj to adjust the denominator if needed
  
K_matrix_cluster <- function(eps, cluster_var, df_adj=0){
  ID_list = unique(cluster_var)
  K_matrix = matrix(0, nrow = ncol(eps), ncol = ncol(eps))
  pb = txtProgressBar(min = 0, max = length(ID_list), initial = 0) 
  for(i in 1:length(ID_list)){
    id = ID_list[i]
    cluster_size = sum(cluster_var == id)
    K_matrix = K_matrix + t(matrix(eps[cluster_var == id,], nrow=cluster_size))%*%matrix(eps[cluster_var == id,], nrow = cluster_size) / cluster_size
    setTxtProgressBar(pb,i)
    }
  K_matrix = K_matrix / (length(ID_list) - df_adj)
  return(K_matrix)
}