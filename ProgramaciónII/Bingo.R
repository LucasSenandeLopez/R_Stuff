m_test <- matrix(c(
  c(3,6,8,15,19,2),
  c(1,5,2,4,6,7),
  c(5,11,14,7,17,3),
  c(7,17,18,11,6,14)), byrow = T, nrow=4
)


bingo <- function(mat)
{
  cols <- 6
  rows <- 4
  #mat <- round(matrix(runif(rows*cols, 1, 21), nrow = rows))
      
    
  num_columnas_cero <- 0
  num_filas_cero <- 0
  num <- 0
  
  filas_cero <- c()
  columnas_cero <- c()
  
  
  while(num_columnas_cero < cols)
  {
    row <- 1
    col <- 1
    num <- 0
    
    while(num < 1 || num > 20)
    {
      cat("Introduzca un número entero entre 1 y 20:\n")
      {num <- scan(,integer(),1)}
    }
      
    while(row <= rows) #Se recorre toda la matriz con un solo bucle
    {
      
      if(mat[row, col] == num)
      {
        mat[row,col] <- 0
      }
      
      if(col == cols)
      {
        row <- row + 1
        col <- 1
      } else {
        
        col <- col + 1
      }
      
    }
    
    cat("\n\n")
    print(mat)
    cat("\n\n")
    
    
    
    row <- 1
    col <- 1
    todos_cero <- T
    
    #Se comprueba si la columna está rellenada de ceros
    while(col <= cols)
    {
      row <- 1
      todos_cero <- T
      while(row <= rows && todos_cero)
      {
        if(mat[row, col] != 0){todos_cero <- F}
        row <- row + 1
      }
      
      i <- 1
      columna_no_marcada <- T
      while(i <= length(columnas_cero) && columna_no_marcada)
      {
        if(columnas_cero[i] == col){columna_no_marcada <- F}
        i <- i + 1
      }
      
      
      if(todos_cero && columna_no_marcada)
      {
        cat("Columna", col, "Rellenada!\n")
        columnas_cero <- c(columnas_cero, col)
        num_columnas_cero <- num_columnas_cero + 1
      }
      
      col <- col + 1
    }
    
    row <- 1
    col <- 1
    todos_cero <- T
  
    # Se comprueba si la fila está rellenada de ceros
    while(row <= rows)
    {
      col <- 1
      todos_cero <- T
      
      while(col <= cols && todos_cero)
      {
        if(mat[row, col] != 0){todos_cero <- F}
        col <- col + 1
      }
      
      i <- 1
      fila_no_marcada <- T
      while(i <= length(filas_cero) && fila_no_marcada)
      {
        if(filas_cero[i] == row){fila_no_marcada <- F}
        i <- i + 1
      }
      
      
      if(todos_cero && fila_no_marcada)
      {
        cat("Fila", row, "rellenada!\n")
        filas_cero <- c(filas_cero, row)
        num_filas_cero <- num_filas_cero + 1
      }
      
      row <- row + 1
    }
    
    cat(num_filas_cero, "filas rellenadas\n")
    cat(num_columnas_cero, "columnas rellenadas\n")
      
  }
  
  cat("BINGO\n\n")
  print(mat)
}