mat <- matrix(c(1.65, 1.74, 1.7, 1.55, 1.75, 1.58,65, 80, 77, 60, 66, 65), ncol=2, byrow=F)

colnames(mat) <- c("Altura", "Peso")
rownames(mat) <- c("Ana","Pepe","Nacho","Bea","Gema","Alba")

ex_df <- function(mat)
{
  rows <- nrow(mat)
  
  mat <- data.frame(mat)
  mat$Sexo <- c("M","H","H","M","M","M")
  mat$Edad <- c(23,43,34,43,45,54)
  mat$IMC <- mat$Peso / (mat$Altura)^2
  
  mat$NivelPeso <- rep("", rows)
  
  for(i in 1:rows)
  {
    if(mat$IMC[i] < 18.5)
    {
      mat$NivelPeso[i] <- "Bajo Peso"
       
    } else if (mat$IMC[i] < 25) {
      
      mat$NivelPeso[i] <- "Normal"
      
    } else if (mat$IMC[i] < 30) {
      
      mat$NivelPeso[i] <- "Sobrepeso"
      
    } else {
      
      mat$NivelPeso[i] <- "Obeso"
      
    }
  }
  
  print(mat)
  cat("\n\n")
  
  #D) Cuenta de mujeres con cada nivel de peso
  for (niv in c("Bajo Peso", "Normal", "Sobrepeso", "Obeso"))
  {
    cuenta <- 0
    for (i in 1:rows)
    {
      if(mat$Sexo[i] == "M" && mat$NivelPeso[i] == niv){cuenta <- cuenta + 1}
    }
    cat("\nHay",cuenta, " mujeres con nivel de peso \"", niv, "\"\n")  
  }
  
  #E) Edad Calculada
  
  mat$EdadCalculada <- mat$Edad
  
  for(i in 1:rows)
  {
    if(mat$NivelPeso[i] == "Bajo Peso")
    {
      mat$EdadCalculada[i] <- mat$EdadCalculada[i] - 2
      
    } else if (mat$NivelPeso[i] == "Sobrepeso") {
      
      mat$EdadCalculada[i] <- mat$EdadCalculada[i] + 4
      
    } else if (mat$NivelPeso[i] == "Obeso") {
      
      mat$EdadCalculada[i] <- mat$EdadCalculada[i] + 8
      
    }
  }
  cat("\n\n")
  print(mat)
  cat("\n\n")
  #mat
  
  #F) Quitar nivel de peso "Normal"
  mat <- mat[mat$NivelPeso != "Normal",]
  
  print(mat)
  cat("\n\n")
  
  
  #G) Quitar Columna "Edad"; nótese que es sabido que "Edad" es la col. 4 ya que 
  #la pusimos nosotros
  mat <- mat[,-4]
  
  mat
}

ex_df(mat)

