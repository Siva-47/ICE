# evaluation 2: host game

host_game <- function(repeats, P_host){
  
  "
  evaluate the ability of generalization via host game
  
  Inputs:
    repeats: number of repetitions
    P_host: estimate used as the host
  
  Outputs:
    res: RMSE of the methods to evaluate
  "
  
  n <- dim(P_host)[1]
  usvt_rmse <- ns_rmse <- ice_rmse <- rep(0, repeats)
  
  for(r in 1:repeats){
    gen_A <- apply(P_host, c(1, 2), function(x){rbinom(1, 1, x)})
    gen_A[lower.tri(gen_A)] <- 0
    gen_A <- gen_A+t(gen_A)
    diag(gen_A) <- 0
    
    rank_ <- rankMatrix(P_host)
    
    # USVT
    usvt_res <- USVT(gen_A)
    usvt_rmse[r] <- sqrt(mean((usvt_res$P_hat-P_host)^2))
    
    # NS
    ns_res <- NS(gen_A, C = 1)
    ns_rmse[r] <- sqrt(mean((ns_res$P_hat-P_host)^2))
    
    # ICE
    ice_res <- ICE(A = gen_A, C_it = C_it_selected, C_est = C_est_selected, 
                   P_hat_0 = NULL, rounds = 10)
    ice_rmse[r] <- sqrt(mean((ice_res$final_P_hat-P_host)^2))
  }
  
  res <- cbind(usvt_rmse, ns_rmse, ice_rmse)
  
  return(list(res = res))
}


# load data
setwd('C:/Users/yulin/Desktop/ICE-master/') # your path
library(igraph)
g <- read_graph('./real_data/data/sub-0027055_ses-1_dwi_DS00350.graphml', 
                format = 'graphml')
A <- as_adjacency_matrix(as.undirected(g))
A <- as.matrix(A)

# USVT
usvt_res <- USVT(A)
P_usvt <- usvt_res$P_hat

# NS
ns_res <- NS(A, C = 1)
P_ns <- ns_res$P_hat

# ICE
# selecting tuning parameters via network cross-validation
num_it <- 5
num_est <- 5
can_it <- seq(1/3, 5/3, length.out = num_it)
can_est <- seq(1/3, 5/3, length.out = num_est)

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

ice_res <- ICE(A = A, C_it = C_it_selected, C_est = C_est_selected, 
               P_hat_0 = NULL, rounds = 10)
P_ice <- ice_res$final_P_hat

# play the game
repeats <- 3
host_usvt <- host_game(repeats, P_host = P_usvt)
host_ns <- host_game(repeats, P_host = P_ns)
host_ice <- host_game(repeats, P_host = P_ice)

# report the result
show <- data.frame(rmse = c(apply(host_usvt$res, 2, mean),
                            apply(host_ns$res, 2, mean),
                            apply(host_ice$res, 2, mean)),
                   method = rep(c('USVT', 'NS', 'ICE'), 3),
                   host = rep(c('USVT', 'NS', 'ICE'), each = 3))

show$method <- factor(show$method, levels = c('ICE', 'NS', 'USVT'))
show$host <- factor(show$host, levels = c('ICE', 'NS', 'USVT') )

library(data.table)
write.csv(show, './real_data/result/home_game.csv', row.names = FALSE)
show <- fread('./real_data/result/home_game.csv')

library(ggplot2)
ggplot(show, aes(x = host, y = rmse, fill = method))+
  geom_bar(stat = "identity", position = 'dodge')+
  scale_fill_brewer(palette = 'Set1')+
  theme_bw()+
  theme(axis.title.x = element_text(size = 17), axis.title.y=element_text(size=17),
        axis.text = element_text(size = 15), 
        legend.title = element_text(size=15),
        legend.text=element_text(size=14),
        legend.position = 'top')

