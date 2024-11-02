setwd("C:\\Users\\goomb\\OneDrive\\Documentos\\GitHub\\R_Stuff")
ex_1 <- function(mat)
{
  cols_t <- nrow(mat)
  rows_t <- ncol(mat)
  
  mat_t <- matrix(rep(0, cols_t*rows_t), ncol  = cols_t, nrow = rows_t)
 
  for(i in 1:cols_t)
  {
    for(j in 1:rows_t)
    {
      mat_t[j,i] <- mat[i,j]
    }  
  }
   mat_t
}

ex_2 <- function(sq_mat)
{
  dims <- ncol(sq_mat)
  diag <- vector(mode = "integer", length = dims)
  cat("Matriz original:\n")
  print(sq_mat)
  
  for(i in 1:dims)
  {
    diag[i] <- sq_mat[i,i]
    sq_mat[i,i] <- i
  }
  cat("\nDiagonal:\n")
  print(diag)
  cat("\nMatriz ahora:\n ")
  print(sq_mat)
}

ex_3 <- function(mat)
{
  rows <- nrow(mat)
  cols <- ncol(mat)
  mat_nonfinished <- T

  while(mat_nonfinished)
  {
    num <- -1
    while(num < 0 || num > 20)
    {
      cat("Introduzca un número entero entre 0 y 20:\n")
      {num <- scan(,what=integer(),1)}
    }  
      
    
    i <- 1
    row <- 1
    col <- 1
    in_mat <- F
    
    repeat
    {
      
      if(i > rows*cols){break}
      
      if(mat[row,col] == num)
      {
        mat[row,col] <- -1
        in_mat <- T
      }
        
      i <- i + 1

      if(row < rows)
      {
        row <- row + 1
      } else {
        col <- col + 1
        row <- 1
      }
  }
  
    
    if(in_mat)
    {
      cat(num, " está en la matriz\n")
    }
    
    mat_nonfinished <- sum(mat == -1) != rows*cols
  }
  mat
}  

ex_4 <- function(mat)
{
  rows <- nrow(mat)
  cols <- ncol(mat)
  mat_nonfinished <- T
  tiros <- 1
  
  while(mat_nonfinished && tiros <= 10)
  {
    num <- -1
    while(num < 0 || num > 20)
    {
      cat("Introduzca un número entero entre 0 y 20:\n")
      {num <- scan(,what=integer(),1)}
    }  
    
    i <- 1
    row <- 1
    col <- 1
    in_mat <- F
    
    repeat
    {
      if(i > rows*cols){break}
      
      if(mat[row,col] == num)
      {
        mat[row,col] <- -1
        in_mat <- T
      }
      
      i <- i + 1

      if(row < rows)
      {
        row <- row + 1
      } else {
        col <- col + 1
        row <- 1
      }
      
      i <- row + col
      
    }
    
    if(in_mat)
    {
      cat(num, " está en la matriz\n")
    }
    
    mat_nonfinished <- sum(mat == -1) != rows*cols
    tiros <- tiros + 1
  }
  cat("El jugador ha acertado", sum(mat == -1), "tiros\n")
  mat
}  
ex_5 <- function(mat)
{
  max_tiros <- 0
  while(max_tiros < 1)
  {
    cat("introduce el número máximo de tiros")
    {max_tiros <- scan(,integer(),1)}
  }
  
  rows <- nrow(mat)
  cols <- ncol(mat)
  mat_nonfinished <- T
  tiros <- 1
  
  while(mat_nonfinished && tiros <= max_tiros)
  {
    num <- -1
    while(num < 0 || num > 20)
    {
      cat("Introduzca un número entero entre 0 y 20:\n")
      {num <- scan(,what=integer(),1)}
    }  
    
    i <- 1
    row <- 1
    col <- 1
    in_mat <- num == mat[row, col]
    
    repeat
    {
      if(i > rows*cols){break}
      
      if(mat[row,col] == num)
      {
        mat[row,col] <- -1
        in_mat <- T
      }
      
      i <- i + 1

      if(row < rows)
      {
        row <- row + 1
      } else {
        col <- col + 1
        row <- 1
      }
      
    }
    
    if(in_mat)
    {
      cat(num, " está en la matriz\n")
    }
    
    mat_nonfinished <- sum(mat == -1) != rows*cols
    tiros <- tiros + 1
  }
  
  
  
  for(i in 1:nrow(mat))
  {
    write(paste(mat[i,], "\n"),file="nuevo.txt",append=TRUE)
  }
  
  write(paste(sum(mat == -1), "tiros acertados de", max_tiros, "\n"),file="nuevo.txt",append=TRUE)
  
  cat("El jugador ha acertado", sum(mat == -1), "tiros de", max_tiros ,"\n")
  mat
}  

ex_6 <- function(mat)
{
  rows <- nrow(mat)
  cols <- ncol(mat)
  mat_nonfinished <- T
  nums_probados <- c()
  
  while(mat_nonfinished)
  {
    num <- -1
    while(num < 0 || num > 20 || sum(nums_probados == num) > 0)
    {
      cat("Introduzca un número entero no repetido entre 0 y 20:\n")
      {num <- scan(,what=integer(),1)}
      
    }  
    nums_probados <- c(nums_probados, num)
    i <- 1
    row <- 1
    col <- 1
    in_mat <- F
    
    for(row in 1:rows)
    {
      for(col in 1:cols)
      {
        if(mat[row, col] == num)
        {
          in_mat <- T
          mat[row, col] <- -1
        }
      }
    }
    
    if(in_mat)
    {
      cat(num, " está en la matriz\n")
    } else {
      cat(num, " no está en la matriz\n")
    }
      
    
    mat_nonfinished <- sum(mat == -1) != rows*cols
  }
  cat("\n", nums_probados, "\n")
  mat
}  





test_mat <- matrix(c(6,14,13,3,9,5,7,1,21,32,5,2), nrow = 4, byrow = T)
test_sq_mat <- matrix(c(6,14,13,3,9,5,7,1,21), nrow = 3, byrow = T)
small_mat <- matrix(c(2,3,4,5), nrow = 2, byrow  = T)
small_mat_2 <- matrix(c(2,3,3,3), nrow = 2, byrow  = T)

ex_1(test_mat)
ex_2(test_sq_mat)
ex_3(small_mat)
ex_4(small_mat)
ex_5(small_mat)
ex_6(small_mat_2)
