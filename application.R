# generate a network
n <- 1000
gp <- gp_generate(n, setting = 4)
A <- gp$A
P <- gp$P

# two ways to stop the iterations (with given C_it and C_est):
## use delta_P
ice_res <- ICE(A = A, C_it = 0.2, C_est = 1,
               P_hat_0 = NULL, delta_0 = 0.1)
rmse <- sqrt(mean((ice_res$final_P_hat-P)^2))
print(rmse)

## use a fix number of rounds
ice_res <- ICE(A = A, C_it = 0.2, C_est = 1,
               P_hat_0 = NULL, rounds = 10)
rmse <- sqrt(mean((ice_res$final_P_hat-P)^2))
print(rmse)

# selecting tuning parameters
num_it <- 5 
num_est <- 5
can_it <- seq(1/20, 19/20, length.out = num_it) # candidates of C_it
can_est <- seq(1/3, 5/3, length.out = num_est) # candidates of C_est

res_loss <- matrix(0, num_it, num_est)
M = 1 # a small M is adequate
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

# using selected parameters to obtain the estimate
ice_res <- ICE(A = A, C_it = C_it_selected, C_est = C_est_selected, 
               P_hat_0 = NULL, rounds = 10)
rmse <- sqrt(mean((ice_res$final_P_hat-P)^2))
print(rmse)

# view the estimate
library(pheatmap)
library(viridis)
bk <- seq(0, 1, 0.01)
pheatmap(ice_res$final_P_hat,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)






