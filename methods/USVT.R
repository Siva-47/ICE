USVT <- function(A){
  
  "
  apply USVT to get an estimate
  
  Inputs:
    A: adjacency matrix
    
  Outputs:
    P_hat: estimate of the connecting probability matrix
  "
  
  n <- dim(A)[1]
  
  svd <- svd(A)
  num_vector <- sum(svd$d > 1.01*sqrt(n))
  
  if(num_vector == 1){
    D = matrix(svd$d[1], 1, 1)
  }else{
    D = diag(svd$d[1:num_vector])
  }
  
  P_hat <- svd$u[, 1:num_vector]%*%D%*%t(svd$v[, 1:num_vector])
  P_hat <- pmax(pmin(P_hat, 1), 0)
  
  return(list(P_hat = P_hat))
}
