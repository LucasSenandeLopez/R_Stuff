ex_1a <- function(vec)
{
  repeat
  {
    cat("Introduzca un número par\n")
    {num <- scan(,what=integer(), 1)}
    
    if(num %% 2 == 0){break}
  }
  
  count <- 0
  len <- length(vec)
  for(i in 1:len)
  {
    if(vec[i] == num)
    {
      vec[i] <- -1
      count <- count + 1
    }
    
  }
  
  cat("El número ", num, " aparece ", count, " veces\n")
  vec
}


ex_1b <- function(vec)
{
  len <- length(vec)
  nums <- vector("integer", len)
  
  for(j in 1:len)
  {
    repeat
    {
      cat("Introduzca un número par")
      
      {nums[j] <- scan(,what=integer(), 1)}
      
      if(nums[j] %% 2 == 0)
      {
        break #Se sale del repeat y se va al siguiente número
      }
    }
  }
  
  count <- 0
  coincide <- FALSE
  j <- 1
  
  for(i in 1:len)
  {
    j <- 1
    coincide <- FALSE
    repeat
    {
      coincide <- nums[i] == vec[j]
      j <- j + 1
      if(coincide || j > len)
      {
        break
      }
    }
    count <- count + coincide #TRUE se representa como 1
    if(coincide)
    {
      vec[j-1] <- -1
    }
  }
  
  
  cat("Hay ", count, " elementos coincdentes\n")
  vec
}


ex_2 <- function(vec)
{
  vec_ <- vec
  
  A=c("M","D", "C","L","X","V","I") 
  B=c(1000,500,100,50,10,5,1) 
  
  num <- 0
  j <- 1
  len_num <- length(B)
  
  for(i in seq_along(vec))
  {
    
    repeat
    {
      if(vec[i] == A[j])
      {
        vec[i] <- B[j]
        num <- num + B[j]
        break
      }
      j <- j + 1
    }
    j <- 1
  }
  
  cat("El vector ", vec_, " representa en números romanos el número ", num, "\n")
  as.numeric(vec)
  
}