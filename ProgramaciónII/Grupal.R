ex_1a <- function(vec_A, vec_B)
{
  resultado <- c()

  for(i in 1:length(vec_A))
  {
    veces <- 0
    for(j in 1:length(vec_A))
    {
     
      if(vec_A[i] == vec_A[j])
      {
        veces <- veces  + 1
      }
    }
    
    for(j in 1:length(B))
    {
      if(vec_A[i] == vec_B[j])
      {
        veces <- veces  + 1
      }
    }
    
    resultado <- c(resultado, vec_A[i], veces)
  }
}

ex_1b <- function(vec_A, vec_B)
{
  resultado <- c()
  
  for(i in 1:length(vec_A))
  {
    veces <- 0
    for(j in 1:length(vec_A))
    {
      
      if(vec_A[i] == vec_A[j])
      {
        veces <- veces  + 1
      }
    }
    
    for(j in 1:length(B))
    {
      if(vec_A[i] == vec_B[j])
      {
        veces <- veces  + 1
      }
    }
    
    p <- 1
    insertar <- FALSE
    
    while(p <= 2*length((esultado) && insertar)
    {
      if(A[i] == resultado[p]){insertar <- FALSE}
      
      p <- p + 2
    }
  }
}


