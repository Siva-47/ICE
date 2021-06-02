asSymmetric <- function(M){
  M[lower.tri(M)] <- 0
  M <- M+t(M)
  diag(M) <- diag(M)/2
  return(M)
}

gp_generate <- function(n, setting){
  
  "
  generate networks with graphons
  
  Inputs:
    n: number of vertexes
    setting: id of graphon used
  
  Outputs:
    P: connecting probability matrix
    A: adjacency matrix
  "
  
  x <- y <- seq(1/n, 1, 1/n)
  P <- matrix(0, n, n)
  for(i in 1:n){
    for(j in 1:n){
      P[i, j] <- switch(setting, 
                        1/(1/8+exp(5*(x[i]^2 + y[j]^2))),
                        ((x[i]^2+y[j]^2)/3)*cos(1/(x[i]^2+y[j]^2))+0.15,
                        sin(5*pi*(x[i]+y[j]-1))/2+0.5,
                        exp(max(x[i],y[j])^(3/4))*cos(1/(x[i]^2+y[j]^2))/5+0.4,
                        1/(1+exp(10*min(0.2*abs(x[i]-y[j]), max(0.18-(x[i]-1/2)^2-(y[j]-1/2)^2, (x[i]-1/2)^2+(y[j]-1/2)^2-0.245)))),
                        1/4*min(exp(sin(6/((1-x[i])^2+y[j]^2))), exp(sin(6/(x[i]^2+(1-y[j])^2)))),
                        min(exp(0.05-(x[i]-1/2)^2-(y[j]-1/2)^2)*cos(0.4/((x[i]-1/2)^2+(y[j]-1/2)^2))/5,
                            exp(0.0004-(x[i]-1/8)^2-(y[j]-1/8)^2)*cos(0.1/((x[i]-1/8)^2+(y[j]-1/8)^2))/5,
                            exp(0.0004-(x[i]-7/8)^2-(y[j]-7/8)^2)*cos(0.1/((x[i]-7/8)^2+(y[j]-7/8)^2))/5, na.rm = TRUE)+0.4,
                        1/(1+exp(20*min((x[i]-1/4)^2+(y[j]-3/4)^2, (x[i]-1/6)^2+(y[j]-3/8)^2, 
                                        (x[i]-1/2)^2+(y[j]-1/5)^2, (x[i]-3/4)^2+(y[j]-1/4)^2,
                                        (x[i]-5/6)^2+(y[j]-5/8)^2, (x[i]-1/2)^2+(y[j]-4/5)^2,
                                        (x[i]-1/8)^2+(y[j]-1/8)^2, (x[i]-3/8)^2+(y[j]-3/8)^2,
                                        (x[i]-5/8)^2+(y[j]-5/8)^2, (x[i]-7/8)^2+(y[j]-7/8)^2)^(1/2)-0.3)))
    }
  }
  A <- apply(P, c(1, 2), function(x){rbinom(1, 1, x)})
  A <- asSymmetric(A)
  
  return(list(P = P, A = A))
}

# example: generate a network with 200 vertexes using Graphon 4 
gp <- gp_generate(200, 4)
A <- gp$A
P <- gp$P

# view the connecting probability matrix
library(pheatmap)
library(viridis)
bk <- seq(0, 1, 0.01)
pheatmap(P,
         cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE, show_colnames = FALSE,
         scale = "none",
         color = viridis(length(bk), direction = -1),
         legend_breaks=seq(0, 1, 0.1),
         breaks=bk)
