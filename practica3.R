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
  tablero[2, 2] <- NA
  coo <- as.data.frame(which(tablero == esp, arr.ind = T))
  coo <- coo[with(coo, order(coo$row)), ]
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


#Implemente la funci´on indMueve(tablero), que tiene por input un tablero (tablero)y ejecuta 
#la fase de movimiento, recorriendo cada casillero del mismo

indMueve <- function(tablero){
  n <- nrow(tablero)
  for(i in (2:(n-1))){
    for(j in (2:(n-1))){
      
      if (tablero[i, j] %in% c(1, -1)){
        bicho    <- tablero[i, j]
        centro   <- c(i, j)
        nPos     <- buscarAdyacente(tablero,centro, esp = 0)
        
        if(nPos[1] != "NO"){
          tablero[i, j]                   <- 0
          tablero[nPos[1, 1], nPos[1, 2]] <- 2*bicho
        }
      }
    }
  }
  tablero[tablero == -2] <- -1
  tablero[tablero == 2]  <- 1
  return(tablero)
}

#Implemente la funci´on depredCome(tablero) que tiene por input un tablero (tablero)
#y ejecuta la fase de alimentaci´on recorriendo cada casillero del mismo. En cada paso,
#verifica si la posici´on (i, j) elegida esta ocupada por un tibur´on y, en tal caso, busca la
#primer foca dispoible en la vecindad. Si la encuentra, el tibur´on se mueve a la posici´on
#ocupada por la foca, y la devora

depredCome <- function(tablero){
  n <- nrow(tablero)
  for(i in (2:(n-1))){
    for(j in (2:(n-1))){
      if (tablero[i, j] == -1){
        centro <- c(i, j)
        nPos <- buscarAdyacente(tablero,centro, esp = 1)
        if(nPos[1] != "NO"){
          tablero[i, j] <- 0
          tablero[nPos[1, 1], nPos[1, 2]] <- -2
        }
      }
    }
  }
  tablero[tablero == -2] <- -1
  return(tablero)
}

#Implemente la funci´on reproduccion(tablero) que tiene por input un tablero (tablero)
#y ejecuta la fase de reproducci´on recorriendo cada casillero del mismo. En cada paso,
#verifica si la posici´on (i, j) elegida esta ocupada. En caso de estarlo, busca en la vecindad
#si hay otro individuo de su misma especie, en caso de encontrarlo, busca la primer
#posici´on vac´ıa y hace aparecer en esa posici´on un nuevo individuo de la misma especie


reproduccion <- function(tablero){
  n <- nrow(tablero)
  for(i in (2:(n-1))){
    for(j in (2:(n-1))){
      if (tablero[i, j] %in% c(1, -1)){
        bicho <- tablero[i, j]
        centro <- c(i, j)
        mV <- misVecinos(tablero, centro)
        nPos <- as.data.frame(buscarAdyacente(tablero,centro, esp = bicho))
        if((nPos[1, 1] != "NO") & (sum(mV == 0) != 0)){
          nPosVacio <- as.data.frame(buscarAdyacente(tablero,centro, esp = 0))
          tablero[i, j] <- 2*(bicho)
          tablero[nPosVacio[1, 1], nPosVacio[1, 2]] <- 2*(bicho)
        }
      }
    }
  }
  tablero[tablero == 2] <- 1
  tablero[tablero == -2] <- -1
  return(tablero)
}

#Simule la evoluci´on del sistema durante 2 ciclos partiendo del siguiente tablero inicial, definido en el item 4

ciclos <- function(tablero, n){
  for(i in 1:n){
    tablero <- indMueve(tablero)
    tablero <- depredCome(tablero)
    tablero <- reproduccion(tablero)
  }
  return(tablero)
}

sum(ciclos(miTablero, n = 2) == 1)


#Estudie si las focas se extinguen durante los primeros N=10 ciclos, y en tal caso, indique
#en que ciclo se produce la extinci´on.

for(i in 1:10){
  li <- ciclos(miTablero, n = i)
  if (sum(li == 1) == 0){
    break
  }
}
i

#Agregado mio... quiero ver que ocurre si en cada ciclo se agrega un paso de muerte natural, en el que tanto 
#focas como tiburones tengan una cierta probabilidad de morir... ERGO: Flashiadas

muerteNatural <- function(tablero, p){
  n_focas               <- sum(tablero == 1)  
  muerte_focas          <- sample(c(0, 1), n_focas, replace = T, prob = c(p, (1 - p)))
  tablero[tablero == 1] <- muerte_focas
  
  n_tibu                 <- sum(tablero == -1)  
  muerte_tibu            <- sample(c(0, -1), n_tibu, replace = T, prob = c(p, (1 - p)))
  tablero[tablero == -1] <- muerte_tibu
  
  return(tablero)
}

ciclos_pred_presa_con_muerteNatural <- function(tablero, n, p){
  for(i in 1:n){
    tablero <- indMueve(tablero)
    tablero <- depredCome(tablero)
    tablero <- reproduccion(tablero)
    tablero <- muerteNatural(tablero, p)
  }
  return(tablero)
}

extincion_focas <- function(tablero, p){
  n = nrow(tablero)
  i=1
  while(sum(tablero == 1) != 0){
    if (sum(tablero == -1) >= 2){
      tablero <- ciclos_pred_presa_con_muerteNatural(tablero, n = i, p)
    } else {
      return(-1)
      }
    i = i+1
  }
  return(i)
}

simuPred_Presa <- replicate(10, extincion_focas(miTablero, p = .1))


