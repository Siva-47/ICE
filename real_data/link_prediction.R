# evaluation 1: link prediction

gen_miss <- function(A_true, p = 0.1){
  
  "
  generate observed adjacency matrix with missing edges
  
  Inputs:
    A_true: full adjacency matrix
    p: missing rate
  
  Outputs:
    M: missing matrix
    A_obs: observed adjacency matrix 
    A_true: full adjacency matrix
  "
  
  n <- dim(A_true)[1]
  M <- matrix(rbinom(n*n, 1, 1-p), n, n)
  M[upper.tri(M)] <- 0
  M <- M + t(M)
  diag(M) <- 1
  A_obs <- M * A_true
  
  return(list(M = M, A_true = A_true, A_obs = A_obs))
}

eva <- function(P_hat, M, A_true, t){
  
  "
  get FP and TP with given threshold t
  
  Inputs:
    P_hat: estimate
    M: missing matrix
    A_true: full adjacency matrix
    t: threshold
  
  Outputs:
    fp: false positive rate
    tp: true positive rate
  "
  
  fp <- sum(P_hat > t & M == 0 & A_true == 0) / sum(M == 0 & A_true == 0)
  tp <- sum(P_hat > t & M == 0 & A_true == 1) / sum(M == 0 & A_true == 1)
  
  return(list(fp = fp, tp = tp))
}


# load data
setwd('/ICE-master/') # your path
library(igraph)
g <- read_graph('./real_data/data/sub-0027055_ses-1_dwi_DS00350.graphml', 
                format = 'graphml')
A <- as_adjacency_matrix(as.undirected(g))
A <- as.matrix(A)
n <- dim(A)[1]

repeats <- 10
rec_ns <- rec_usvt <- rec_ice <- matrix(0, repeats*102, 2)

num_it <- 5
num_est <- 5
can_it <- seq(1/3, 5/3, length.out = num_it)
can_est <- seq(1/3, 5/3, length.out = num_est)

for(r in 1:repeats){
  set.seed(r)
  miss = gen_miss(A)
  rank_ <- rankMatrix(miss$A_obs)
  
  # ICE
  # selecting tuning parameters via network cross-validation
  num_it <- 5
  num_est <- 5
  can_it <- seq(1/3, 5/3, length.out = num_it)
  can_est <- seq(1/3, 5/3, length.out = num_est)
  
  M = 1
  res_loss <- matrix(0, num_it, num_est)
  for(m in 1:M){
    res <- validation_ICE(miss$A_obs, p = 0.9,
                          can_it = can_it,
                          can_est = can_est)
    res_loss <- res_loss + res$loss_
  }
  id <- which(res_loss == min(res_loss), arr.ind = TRUE)
  idx_1 <- id[1]
  idx_2 <- id[2]
  C_it_selected <- can_it[idx_1]
  C_est_selected <- can_est[idx_2]
  
  ice_res <- ICE(A = miss$A_obs, 
                 C_it = C_it_selected, C_est = C_est_selected, 
                 P_hat_0 = NULL, rounds = 10)
  
  for(t in 1:102){
    res <- eva(ice_res$final_P_hat, miss$M, miss$A_true, 0.01*(t-2))
    rec_ice[(r-1)*102+t, ] <- c(res$tp, res$fp)
  }
  
  ns_res <- NS(miss$A_obs, C = 1)
  for(t in 1:102){
    res <- eva(ns_res$P_hat, miss$M, miss$A_true, 0.01*(t-2))
    rec_ns[(r-1)*102+t, ] <- c(res$tp, res$fp)
  }
  
  usvt_res <- USVT(miss$A_obs)
  for(t in 1:102){
    res <- eva(usvt_res$P_hat, miss$M, miss$A_true, 0.01*(t-2))
    rec_usvt[(r-1)*102+t, ] <- c(res$tp, res$fp)
  }
}

rec_ns_ <- rec_usvt_ <- rec_ice_ <- matrix(0, 102, 2)
for(r in 1:repeats){
  rec_ns_ <- rec_ns_+rec_ns[((r-1)*102+1):(r*102), ]
  rec_usvt_ <- rec_usvt_+rec_usvt[((r-1)*102+1):(r*102), ]
  rec_ice_ <- rec_ice_+rec_ice[((r-1)*102+1):(r*102), ]
}
rec_ns_ <- rec_ns_/repeats
rec_usvt_ <- rec_usvt_/repeats
rec_ice_ <- rec_ice_/repeats

show <- data.frame(rbind(rec_ice_, rec_ns_, rec_usvt_))
colnames(show) <- c('tp', 'fp')
show$method <- rep(c('ICE', 'NS', 'USVT'), each = 102)
show$method <- factor(show$method, levels = c('ICE', 'NS', 'USVT'))

library(data.table)
write.csv(show, './real_data/result/link_pre.csv', row.names = FALSE)
show <- fread('./real_data/result/link_pre.csv')

library(ggplot2)
ggplot(show, aes(x = fp, y = tp, color = method, linetype = method))+
  geom_line(size = 1.2)+
  scale_color_brewer(palette = 'Set1')+
  labs(x = 'FP', y = 'TP', color = 'Method', linetype = 'Method')+
  theme_bw()+
  theme(axis.title.x = element_text(size = 17), axis.title.y=element_text(size=17),
        axis.text = element_text(size = 15), 
        legend.title = element_text(size=15),
        legend.text=element_text(size=14),
        legend.position = c(0.85, 0.35),
        legend.background = element_rect(fill = 'white', colour = 'black'))



