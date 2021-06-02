setwd('C:/Users/yulin/Desktop/ICE-master/') # your path

n <- 1000
repeats <- 3 # 3 repetitions for quick display
settings <- 1:8

for(s in settings){
  C_it_selected_record <- C_est_selected_record <- rep(0, repeats)
  Oracle_est_selected_record <- rep(0, repeats)
  ice_rmse <- ns_rmse <- usvt_rmse <- Oracle_rmse <- rep(0, repeats)
  
  for(r in 1:repeats){
    set.seed(r)
    print(paste('repetition', r))
    gp <- gp_generate(n, s)
    P <- gp$P
    A <- gp$A
    
    write.csv(P, paste0('./simulation/data/gp', s, '_P', '_', r, '.csv'), row.names = FALSE)
    write.csv(A, paste0('./simulation/data/gp', s, '_A', '_', r, '.csv'), row.names = FALSE)
    
    num_it <- 5
    num_est <- 5
    if(s %in% c(1, 2, 3, 7)){
      can_it <- seq(1/20, 37/20, length.out = num_it)
    }else if(s %in% c(4, 5, 6, 8)){
      can_it <- seq(1/20, 19/20, length.out = num_it)
    }
    
    if(s %in% c(1, 2)){
      can_est <- seq(1, 11/3, length.out = num_est)
    }else if(s %in% c(3, 5, 7, 8)){
      can_est <- seq(1/3, 9/3, length.out = num_est)
    }else if(s %in% c(4, 6)){
      can_est <- seq(1/3, 5/3, length.out = num_est)
    }
    
    # USVT
    usvt_res <- USVT(A)
    usvt_rmse[r] <- sqrt(mean((usvt_res$P_hat-P)^2))
    
    # NS
    ns_res <- NS(A, C = 1)
    ns_rmse[r] <- sqrt(mean((ns_res$P_hat-P)^2))
    
    # ICE
    # selecting tuning parameters via network cross-validation
    M = 1
    res_loss <- matrix(0, num_it, num_est)
    for(m in 1:M){
      res <- validation_ICE(A, p = 0.9,
                            can_it = can_it,
                            can_est = can_est)
      res_loss <- res_loss + res$loss_
    }
    id = which(res_loss == min(res_loss), arr.ind = TRUE)
    idx_1 = id[1]
    idx_2 = id[2]
    C_it_selected <- can_it[idx_1]
    C_est_selected <- can_est[idx_2]
    C_it_selected_record[r] <- C_it_selected
    C_est_selected_record[r] <- C_est_selected
    
    ice_res <- ICE(A = A, C_it = C_it_selected, C_est = C_est_selected, 
                   P_hat_0 = NULL, rounds = 10)
    ice_rmse[r] <- sqrt(mean((ice_res$final_P_hat-P)^2))
    
    # Oracle
    Oracle_rmse_ <- rep(0, num_est)
    for(l in 1:num_est){
      Oracle_res <- Oracle(A, P, C = can_est[l])
      Oracle_rmse_[l] <- sqrt(mean((Oracle_res$P_hat-P)^2))
    }
    Oracle_rmse[r] <- min(Oracle_rmse_)
    Oracle_est_selected_record[r] <- can_est[which.min(Oracle_rmse_)]
  }
  
  result <- data.frame(ice = ice_rmse, ns = ns_rmse, 
                       usvt = usvt_rmse, oracle = Oracle_rmse)
  select <- data.frame(ice_it = C_it_selected_record,
                       ice_est = C_est_selected_record,
                       Oracle_est = Oracle_est_selected_record)
  
  write.csv(result, paste0('./simulation/result/gp', s, '.csv'), row.names = FALSE)
  write.csv(select, paste0('./simulation/result/gp', s, '_select.csv'), row.names = FALSE)
}
