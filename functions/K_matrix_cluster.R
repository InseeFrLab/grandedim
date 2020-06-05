#' Compute the middle of the sandwich clustered by the specific variable
#' 
#' Created: 05/06/2020
#' 
#' @param eps n x t matrix of residuals with row equal to X_{i,t} vareps_{i,t}
#' @param cluster_var variable that identifies the cluster, dimension n
#' @param dj_adj to adjust the denominator if needed
#' 
#' @author Jérémy L'Hour
  
K_matrix_cluster <- function(eps, cluster_var, df_adj=0){
  ID_list = unique(cluster_var) # unique cluster identifiers
  K_matrix = matrix(0, nrow = ncol(eps), ncol = ncol(eps))
  pb = txtProgressBar(min = 0, max = length(ID_list), initial = 0) 
  for(i in 1:length(ID_list)){
    id = ID_list[i] # current cluster id
    cluster_size = sum(cluster_var == id) # cluster size
    K_matrix = K_matrix + t(matrix(eps[cluster_var == id,], nrow=cluster_size))%*%matrix(eps[cluster_var == id,], nrow = cluster_size) / cluster_size
    setTxtProgressBar(pb,i)
    }
  K_matrix = K_matrix / (length(ID_list) - df_adj) # divide by the right thing
  return(K_matrix)
}