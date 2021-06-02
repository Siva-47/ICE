library(data.table)
library(igraph)

# load data
setwd('C:/Users/yulin/Desktop/ICE-master/') # your path
g <- read_graph('./real_data/data/sub-0027055_ses-1_dwi_DS00350.graphml', 
                format = 'graphml')
A <- as_adjacency_matrix(as.undirected(g))
A <- as.matrix(A)
write.csv(A, './real_data/data/A.csv', row.names = FALSE)

# view the adjacency matrix
library(pheatmap)
library(viridisLite)
bk = seq(0, 1, 0.01)
pheatmap(A,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

# USVT
usvt_res <- USVT(A)
pheatmap(usvt_res$P_hat,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

# NS
ns_res <- NS(A, C = 1)
pheatmap(ns_res$P_hat,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

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

pheatmap(ice_res$final_P_hat,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

# SAS
sas <- as.matrix(fread('./real_data/result/sas.csv'))
pheatmap(sas,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

# SBA
sba <- as.matrix(fread('./real_data/result/sba.csv'))
pheatmap(sba,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)

