Oracle <- function(A, P, C = 1){
  
  "
  get an estimate by neighborhood averaging with true neighbors
  
  Inputs:
    A: adjacency matrix
    P: connecting probability matrix
    C: neighborhood proportion
  
  Outputs:
    P_hat: estimate of the connecting probability matrix
  "

  n <- dim(A)[1]
  size <- round(C*sqrt(n*log(n)))
  
  S <-  neighbor(P_hat = P, size = size)
  P_hat <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
  
  return(list(P_hat = P_hat))
}



