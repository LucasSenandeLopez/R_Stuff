get_max_element <- function(vec)
{
  max_idx <- 1
  len <- length(vec)
  
  for(i in 2:len)
  {
    if(vec[i] > vec[max_idx]){max_idx <- i}
  }
  return(vec[max_idx])
}

element_index_vec <- function(vec, element)
{
  len <- length(vec)
  
  return_idx <- 0
  i <- 1
  
  while(i <= length(vec) && return_idx == 0)
  {
    if(vec[i] == element){return_idx <- i}
    i <- i + 1
  }
  
  return(return_idx)
}



ex_1 <- function(vec_A, vec_B)
{
  len_A <- length(vec_A)
  len_B <- length(vec_B)
  
  if(len_A > len_B)
  {
    max_A <- get_max_element(vec_A)
    remaining_elements_to_add <- len_A - len_B
    
    while(remaining_elements_to_add > 0)
    {
     
      num <- max_A + 1
      
      while(num > max_A)
      {
        cat("Por favor, introduzca un número menor o igual a", max_A,"\n")
        {num <- scan(,integer(),1)}
      }
      
      vec_B <- c(vec_B,num)
      
      remaining_elements_to_add <- remaining_elements_to_add - 1 
    }
    
    
  } else if (len_B > len_A) {
    
    remaining_elements_to_remove <- len_B - len_A
    idx_remove <- 0
    element_remove <- 0
    
    while(remaining_elements_to_remove > 0)
    {
      cat("Introduzca un elemento para quitar del vector B:\n")
      {element_remove <- scan(,integer(),1)}
      
      idx_remove <- element_index_vec(vec_B, element_remove)
      
      if(idx_remove != 0)
      {
        vec_B <- vec_B[-idx_remove]
        remaining_elements_to_remove <- remaining_elements_to_remove -1
      }
    }
  }
  
  cat("Vector A:\n")
  print(vec_A)
  cat("\nVector B:\n")
  print(vec_B)
}


test_a_1 <- c(1, 3, 4, 5, 7, 8, 9, 7)
test_b <- c(2, 4, 2, 6, 9, 4)
test_a_2 <- c(1, 3, 4, 5)




