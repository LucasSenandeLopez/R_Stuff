identif_nombre <- function()
{
  name <- readline("Por favor, introduzca su nombre:\n")
  return(name)
}

identif_edad <- function()
{
  edad <- -1
  cat("Por favor introduzca su edad (mayor a 0):\n")
  
  while(edad <= 0)
  {
    {edad <- scan(,integer(),1)}
  }
  
  return(edad)
}

datos_validos <- function(df)
{
  return(df$Edad[1] > 0 && df$Nombre[1] != "")
}

jugar_tres_veces <- function(df)
{
  cat("Por favor", df$Nombre[1], "introduzca tres puntuaciones mayoresn o iguales a 0:\n")
  
  puntuaciones_tres <- c(0,0,0)
  
  for(i in 1:3)
  {
    punt <- -1
    
    while(punt < 0)
    {
      cat("Introduzca la puntuación",i,"mayor o igal a 0:\n")
      {punt <- scan(,integer(),1)}
    }
    
    puntuaciones_tres[i] <- punt
  }
  
  return(puntuaciones_tres)
}

puntuacion_maxima <- function(puntuaciones_vec)
{
  return(max(puntuaciones_vec))
}

juego <- function()
{
  opcion <- 0
  jugador <- data.frame(Nombre=c(""),Edad=c(0), Highscore = c(0))
  puntuaciones <- c(0)
  
  repeat
  {
    while(opcion < 1 || opcion > 4)
    {
      cat("Por favor, seleccione una opción:\n")
      cat("1: Identificarse\n")
      cat("2: Jugar\n")
      cat("3: Mostrar Estadísticas\n")
      cat("4: Terminar\n")
      
      {opcion <- scan(,integer(),1)}
    }
    
    if(opcion == 1)
    {
      jugador$Nombre[1] <- identif_nombre()
      jugador$Edad[1] <- identif_edad()
      
      cat("Registrado el jugador",jugador$Nombre[1],"con",jugador$Edad[1],"años de edad,\n\n")
      
    } else if (opcion == 2 && datos_validos(jugador)){
      
      puntuaciones <- c(puntuaciones,jugar_tres_veces(jugador))
      cat("\n\n")
    } else if (opcion == 3 && datos_validos(jugador)){
      
      jugador$Highscore[1] <- puntuacion_maxima(puntuaciones)
      cat("La puntuación máxima es: ", jugador$Highscore[1], "\n\n")
      
    } else if(opcion == 4){

      break
      
    } else {
      
      cat("Necesita introducir sus datos antes de jugar!\n")  
    }
    
    opcion <- 0
  }
  
  cat("El resultado es:\n")
  jugador
  
}