mat <- matrix(c(1,2,1,2,4,2,3,1,4), nrow=3, byrow=T)

ex <- function(mat)
{
  rows <- nrow(mat)
  cols <- ncol(mat)
  
  ret_mat <- c()
  
  for(row in 1:rows)
  {
    for(col in 1:cols)
    {
      i <- 1
      count <- 0
      already_there <- F
      
      #Comprueba si el elemento ya está en matriz de retorno
      while(i <= length(ret_mat[,1]) && !already_there)
      {
        if(ret_mat[i,1] == mat[row,col]){already_there <- T}
        i <- i + 1
      }
      
      # Si el elemento es nuevo en la matriz hasta ahora,se cuentan todas las
      # veces que aparece desde entonces para añadirlo a la matriz de retorno
      if(!already_there)
      {
        #No hace falta empezar desde el principio porque si el número estuviera
        #antes, ya se hubiera contabilizado
        for(row2 in row:rows)
        {
          for(col2 in 1:cols)
          {
           if(mat[row2,col2] == mat[row,col]){count <- count + 1} 
          }
        }
        ret_mat <- rbind(ret_mat, c(mat[row,col], count))
      }
      
      
  
    }
  }
  
  ret_mat
}

ex(mat)
