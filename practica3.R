#############################################################################################################
#---------------------------------------------Práctica 3----------------------------------------------------#
#############################################################################################################

#Genero una matriz
n<-5
miTablero<-matrix(0, n+2,n+2)

#Le pongo el valor 8 a los bordes lo que va a definir la linea de costa
miTablero[1,]<-rep(8,n+2)
miTablero[n+2,]<-rep(8,n+2)
miTablero[,1]<-rep(8,n+2)
miTablero[,n+2]<-rep(8,n+2)

miTablero[2, 2] = miTablero[3, 3] = miTablero[4, 3] = 1
miTablero[6,6] = miTablero[2, 6] = -1
# Funcion misVecinos(centro) que tenga por input un centro, correspondiente a las coordenadas de un punto 
# del tablero, y devuelva una matriz de 8 × 2que tenga en sus filas las coordenadas vecinas al centro

misVecinos  <- function(tablero = NA, centro = NA){
  f         <- centro[1]-1
  c         <- centro[2]-1
  subMatrix <- tablero[c(f:(f+2)), c(c:(c+2))]
  return(subMatrix)
}


#buscarAdyacente(tablero, centro, esp) que toma como inputs un tablero (tablero), las coordenadas del centro
#de b´usqueda (centro) y que buscar (esp), que puede ser -1 (T), 1 (F) o un 0 (lugar vac´ıo). La funci´on debe devolver
#las coordenadas del primer vecino donde se encuentra la especie pedida, o el valor "NO"
#si la especie buscada no se encuentra en la vecindad.

auxPrimerVecino <- function(tablero = NA, esp = NA){
  coo <- which(tablero == esp, arr.ind = T)
  if(nrow(coo)>0){
    return(coo[1, ])
  } else{
    return("NO")
  }
}

buscarAdyacente <- function(tablero = NA, centro = NA, esp = NA){
  f         <- centro[1]-2
  c         <- centro[2]-2
  
  cnt <- misVecinos(tablero, centro)
  cnt_coo <- auxPrimerVecino(cnt, esp)
  if(length(cnt_coo)>1){
    return(c(f, c) + cnt_coo)
  } else{
    return("NO")
  }
  
}



