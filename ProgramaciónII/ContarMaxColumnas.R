mat <- matrix(c(12,2,32,4,5,6,99,22), nrow = 2, byrow = F)
mat_2 <- matrix(c(1,9,11,99,13,5,9,8), nrow = 2, byrow = F)


ex_1 <- function(mat)
{
  cols <- ncol(mat)
  count <- 0
  col <- 1
  
  if(is.null(cols)){cols <- 0}
  
  cat("Introduzca un número\n")
  {num <- scan(,what=integer(),1)}
  
  repeat
  {
    if(col > cols){break}
    count <- count + (max(mat[,col]) <= num)
    
    col <- col + 1
  }
  
  cat("El máximo de", count, "columnas es menor o igual a", num, "\n")
  
}

ex_2 <- function(mat)
{
  cols <- ncol(mat)
  rows <- nrow(mat)
  count <- 0
  col <- 1

  
  if(is.null(cols)){cols <- 0}
  
  cat("Introduzca un número\n")
  {num <- scan(,what=integer(),1)}
  
  repeat
  {
    if(col > cols){break}
    no_menor <- T
    row <- 1
    
    while(row <= rows && no_menor)
    {
      if(mat[row,col] < num)
      {
        no_menor <- F
        count <- count + 1
      }
      row <- row + 1
    }
    
    
    col <- col + 1
  }
  
  cat("El mínimo de", count, "columnas es menor o igual a", num, "\n")
  
}

