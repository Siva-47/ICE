distance_NS <- function(A){
  
  "
  get the pairwise distances with adjacency matrix
  
  Inputs:
    A: adjacency matrix
    
  Outputs:
    D: distance matrix
  "
  
  n <- dim(A)[1]
  inner_product_M <- A%*%A
  dis_row <- function(index){
    inner_product_M_row <- inner_product_M[index, ]
    temp <- abs(t(inner_product_M_row-t(inner_product_M)))/n
    diag(temp) <- 0
    temp[, index] <- 0
    dis_row_data <- apply(temp, 1, max)
    dis_row_index <- apply(temp, 1, which.max)
    return(list(dis_row_data = dis_row_data, dis_row_index = dis_row_index))
  }
  dis_row_result <- lapply(1:n, dis_row)
  D <- sqrt(matrix(unlist(lapply(dis_row_result, "[", "dis_row_data")), n, n))

  return(list(D = D))
}

neighbor_NS <- function(D, C){
  
  "
  get the neighborhood sets with pairwise distances
  
  Inputs:
    D: distance matrix
    C: neighborhood proportion
    
  Outputs:
    S: neighborhood sets
  "
  
  n <- dim(D)[1]
  h <- sqrt(log(n)/n) * C
  each_neighbor <- function(x, h){
    index <- which(x <= quantile(x, h))
    return(index)
  }
  S <- apply(D, 1, each_neighbor, h = h)
  return(S)
}

estimate_with_neighbor <- function(A, S, quick = TRUE){
  
  "
  update the estimate with the current neighborhood sets S
  
  Inputs:
    A: adjacency matrix
    S: neighborhood sets
    quick: whether to use quick estimation
  
  Outputs:
    P_hat: new estimate
  "
  n <- dim(A)[1]
  P_hat <- matrix(0, n, n)
  
  if(quick == TRUE){
    # quick estimation proposed by (Zhang et al.,2017)
    for(i in 1:n){
      index <- S[[i]]
      P_hat[i, ] <- colmeans(A[index, ]) 
    }
    P_hat <- (P_hat+t(P_hat))/2 
  }else{
    # our proposed estimation
    for(i in 1:n){
      for(j in 1:n){
        index_i <- S[[i]]
        index_j <- S[[j]]
        overlap <- intersect(index_i, index_j)
        P_hat[i, j] <- sum(A[index_i, index_j])/(length(index_i)^2-length(overlap))
      }
    }
  }
  
  return(P_hat)
}

NS <- function(A, C = 1){
  
  "
  apply NS to get an estimate
  
  Inputs:
    A: adjacency matrix
    C: neighborhood proportion
  
  Outputs:
    P_hat: estimate of the connecting probability matrix
    S: neighborhood sets
  "
  
  dis <- distance_NS(A)
  S <- neighbor_NS(dis$D, C)
  P_hat <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
  
  return(list(S = S, P_hat = P_hat))
}
