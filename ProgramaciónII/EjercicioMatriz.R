mat <- matrix(c(
  c(1,5,3,1),
  c(2,1,4,2),
  c(3,1,2,3),
  c(4,2,6,4)), byrow=T, nrow=4)

vec_1 <- c(1,1,3,4)
vec_2 <- c(2,10,3,7)
vec_3 <- c(1,1,1,1)


ex <- function(mat, vec)
{
  n_diag <- length(vec)
  i <- 1
  count <- 0
  
  
  print(vec)
  cat("\n\n")
  
  
  while(i <= n_diag)
  {
    j <- 1
    
    while(j <= n_diag)
      {
        if(mat[i,i] == vec[j])
        {
          tmp <- vec[j]
          vec[j] <- -1
          
          add <- T
          k <- 1
          
          while(k <= n_diag)
          {
            if(vec[k] == tmp){add <- F}
            k <- k + 1
          }
          
          count <- count + add
        }
      j <- j + 1
      }
    i <- i + 1
  }
  
  print(mat)
  cat("\n\n")
  print(vec)
  cat("\n\n")
  cat("Hay", count, "elementos coincidentes únicos en la diagonal")
  
}