tabla <- matrix(c(
  c(1,500,2,750,9,300),
  c(3,400,6,600,8,450),
  c(4,850,5,850,7,650))
,byrow=T, nrow = 3)

colnames(tabla) <- c("Orden","Retribución","Orden","Retribución","Orden","Retribución")


ordenar <- function(mat)
{
  result <- c()
  
  n_rets <- nrow(mat) * (ncol(mat)/2)
  idxs_cols <- seq(1,ncol(mat),2)
  rows <- nrow(mat)
  
  for(i in 1:n_rets)
  {
    
   for(row in 1:rows)
   {
     for(col in idxs_cols)
     {
       if(mat[row, col] == i)
       {
         result <- rbind(result, mat[row, col:(col+1)])
       }
     }   
   } 
    
  }
  
  colnames(result) <- c("Orden", "Retribución")
  print(result)
}