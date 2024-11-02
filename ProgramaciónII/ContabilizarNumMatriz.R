mat <- matrix(c(1,2,1,2,4,2,3,1,4), nrow=3, byrow=T)

ex <- function(mat)
{
 #Se asume que la matriz no está vacía y por tanto
  # La matriz de retorno tampoco
  
  # Se rellena la cuenta como la suma de un array booleano que indica si
  # los elementos son iguales al vector
  ret_mat <- matrix(c(mat[1,1],sum(mat == mat[1,1])), ncol=2, byrow=T)
  
  # Al no haber ningún criterio que fuerce a iterar por columnas o filas
  # en concreto, se puede hacer directamente sobre elementos
  for(element in mat)
  {
    #Si ningún miembro de ret_mat es este elemento de la matriz...
    if(sum(ret_mat[,1] == element) == 0)
    {
      # Se añade este elemento  retmt junto a su fila
      ret_mat <- rbind(ret_mat, c(element, sum(mat == element)))
    }
  }
  
  ret_mat
}

ex(mat)
