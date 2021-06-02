library(Rfast)
library(Matrix)

random_init <- function(n, size){
  
  "
  get random neighbors
  
  Inputs:
    n: number of vertexes
    size: size of neighborhood sets
    
  Outputs:
    S: random neighborhood sets
  "
  
  S <- list()
  for(i in 1:n){
    S[[i]] <- sample((1:n)[-i], size = size, replace = FALSE)
  }
  
  return(S)
}

neighbor <- function(P_hat, size){
  
  "
  get neighbors with current estimate P_hat
  
  Inputs:
    P_hat: the current estimate
    size: size of neighborhood sets
  
  Outputs:
    S: the new neighborhood sets
  "
  
  n <- dim(P_hat)[1]
  D <- as.matrix(Dist(P_hat, method = "euclidean"))
  
  each_neighbor <- function(x){
    index <- which(x <= sort(x)[size+1])
    return(index)
  }
  
  S <- apply(D, 1, each_neighbor)
  
  temp <- list()
  if(is.matrix(S)){
    for(i in 1:n){
      temp[[i]] <- S[, i][which(S[, i] != i)]
    }
    S <- temp
  }else{
    for(i in 1:n){
      S[[i]] <- S[[i]][which(S[[i]] != i)]
    }
  }
  
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


ICE <- function(A, C_it, C_est, P_hat_0 = NULL, delta_0, rounds = NULL){
  
  "
  apply ICE with two-stage strategy to get an estimate
  
  Inputs:
    A: adjacency matrix
    C_it: C used during iterations
    C_est: C used for final estimation
    P_hat_0: initial estimate
    delta_0: the metric to stop the iterations
    rounds: the rounds of the iterations
  
  Outputs:
    P_hat_list: estimate in each iteration
    final_P_hat: final estimate
    final_S: final neighborhood sets
  "
  
  n <- dim(A)[1]
  size_it <- round(C_it*sqrt(n*log(n)))
  size_est <- round(C_est*sqrt(n*log(n)))
  
  # use random initial value if not provided
  if(is.null(P_hat_0)){
    S <- random_init(n, size = size_it)
    P_hat_0 <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
  }
  
  delta_P <- Inf
  P_hat_list <- list()
  P_hat_list[[1]] <- P_hat_0
  
  if(!is.null(rounds)){
    # fix the rounds
    for(m in 1:rounds){
      S <- neighbor(P_hat = P_hat_list[[m]], size = size_it) 
      P_hat_list[[m+1]] <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
      delta_P <- norm(P_hat_list[[m+1]]-P_hat_list[[m]], 'F')/norm(P_hat_list[[m]], 'F')
      print(paste('round', m, 'delta_P =', delta_P))
    }
  }else{
    # stop the iterations according to delta_P
    m <- 0
    while(delta_P > delta_0){
      S <- neighbor(P_hat = P_hat_list[[m+1]], size = size_it)
      P_hat_list[[m+2]] <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
      delta_P <- norm(P_hat_list[[m+2]]-P_hat_list[[m+1]], 'F')/norm(P_hat_list[[m+1]], 'F')
      m <- m+1
      print(paste('round', m, 'delta_P =', delta_P))
    }
  }
  
  final_S <- neighbor(P_hat = P_hat_list[[m+1]], size = size_est)
  final_P_hat <- estimate_with_neighbor(A = A, S = final_S, quick = TRUE)
  
  return(list(P_hat_list = P_hat_list, final_P_hat = final_P_hat, final_S = final_S))
}

validation_ICE <- function(A, p = 0.9, can_it, can_est){
  
  "
  selecting tuning parameters for ICE via network cross-validation
  
  Inputs:
    A: adjacency matrix
    p: proportion of the edges in the training set
    can_it: candidates of C used during iterations
    can_est: candidates of C used for final estimation
  
  Outputs:
    loss_: the log-likelihood loss of all the combinations
    rmse_: the RMSE of all the combinations
  "
  
  n <- dim(A)[1]
  rank_ <- rankMatrix(A)
  
  # random split of training set and validation set
  is_in <- rbinom(n*n, 1, p)
  in_matrix <- matrix(is_in, n, n)
  in_matrix[lower.tri(in_matrix)] <- 0
  in_matrix <- in_matrix + t(in_matrix)
  diag(in_matrix) <- 0
  
  # generate adjacency matrix for the training set
  A_train <- A*in_matrix
  svd_ <- svd(A_train / p)
  A_hat <- svd_$u[, 1:rank_]%*%diag(svd_$d[1:rank_])%*%t(svd_$v[, 1:rank_])
  
  # get the label of the validation set
  A_val <- list()
  label_val <- c()
  count <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if(in_matrix[i, j] == 0){
        A_val[[count]] <- c(i, j)
        label_val <- c(label_val, A[i, j])
        count <- count+1
      }
    }
  }
  
  num_it <- length(can_it)
  num_est <- length(can_est)
  
  loss_ <- rmse_ <- matrix(0, num_it, num_est)
  
  # evaluate the performance of different combinations on the validation set
  for(i in 1:num_it){
    for(j in 1:num_est){
      print(paste('C_it =', can_it[i], ';', 'C_est =', can_est[j]))
      ice_res <- ICE(A = A_hat, C_it = can_it[i], C_est = can_est[j], P_hat_0 = NULL, rounds = 10)
      phat <- ice_res$final_P_hat
      loss <- 0
      sse <- rep(0, length(label_val))
      label_pre <- rep(0, length(label_val))
      for(l in 1:length(label_val)){
        idx <- A_val[[l]][1]
        jdx <- A_val[[l]][2]
        label_pre[l] <- min(1-10^-8, max(10^-8, phat[idx, jdx]))
        loss <- loss-label_val[l]*log(label_pre[l])-(1-label_val[l])*log(1-label_pre[l])
        sse[l] <- (label_pre[l] - label_val[l])^2
      }
      loss_[i, j] = loss / length(label_val)
      rmse_[i, j] = sqrt(mean(sse))
    }
  }
  
  return(list(loss_ = loss_, rmse_ = rmse_))
}

