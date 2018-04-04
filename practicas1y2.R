##########################################################################################
#--------------------------------Problema Figuritas--------------------------------------#
##########################################################################################

cuantosPaquetes <- function(figus = as.numeric(), paquete = as.numeric(), replace = T){
  album <- rep(0, figus)
  n <- 0
  while(sum(album) != figus){
    nPaquete <- sample(figus, paquete, replace = T)
    album[nPaquete] <- 1
    n=n+1
  }
  return(n)
}

cuantosPaquetes(figus = 640, paquete = 5)

simAlbum1  <- replicate(n = 1000, cuantosPaquetes(figus = 670, paquete = 5))
simAlbum10 <- replicate(n = 10000, cuantosPaquetes(figus = 670, paquete = 5))

mean(simAlbum10)
hist(simAlbum10)


##########################################################################################
#--------------------------------Incendio forestal---------------------------------------#
##########################################################################################

bosque <- rep(0, 10)
bosque <- sample(x = c(-1, 0, 1), size = 50, replace = T)

cuantos <- function(bosque = NA, tipoarbol = NA){
  sumtipo <- sum(bosque == tipoarbol)
  return(sumtipo)
}


brotes <- function(bosque = NA, p = NA){
  nbrotes <- sample(x = c(0, 1), size = cuantos(bosque, tipoarbol = 0), replace = T, prob = c(1-p, p))
  bosque[bosque == 0] <- nbrotes
  return(bosque)
}

rayos <- function(bosque = NA, f = NA){
  nrayos <- sample(x = c(1, -1), size = cuantos(bosque, tipoarbol = 1), replace = T, prob = c(1-f, f))
  bosque[bosque == 1] <- nrayos
  return(bosque)
}


propagacion <- function(bosque = NA){
  bosque_i <- rep(0, length(bosque))
  while(sum(bosque == bosque_i) != length(bosque)){
    bosque_i <- bosque
    
    incendio <- unique(c(which(bosque == -1) -1, which(bosque == -1) +1))
    incendio <- incendio[!(incendio > length(bosque) | incendio < 1)]
    incendio <- incendio[!incendio %in% which(bosque == 0)]
    
    bosque[incendio] <- -1
  }
  return(bosque_i)
}


limpieza <- function(bosque){
  bosque[bosque == -1] <- 0
  return(bosque)
}



anoEnElBosque <- function(bosque = NA, p = NA, f = NA){
  primavera   <- brotes(bosque, p)
  #print(primavera)
  epoca_rayos <- rayos(primavera, f)
  #print(epoca_rayos)
  incendio    <- propagacion(epoca_rayos)
  #print(incendio)
  limpiando   <- limpieza(incendio)
  #print(limpiando)
  
  return(limpiando)
}

cosechaPromedio <- function(p = NA, N = NA, Nrep = NA, bosque = NA, f = NA){
  if(sum(is.na(bosque)) == 1){
    bosque <- rep(0, N)
  }
  if(is.na(f)){
    f <- .2
  }
  bosque_i <- bosque
  prodAnual <- c()
  for(i in 1:Nrep){
    bosque_i     <- anoEnElBosque(bosque_i, p, f)
    prodAnual[i] <- cuantos(bosque_i,  tipoarbol = 1)
  }
  return(mean(prodAnual))
}

p <- seq(0, 1, by = 0.01)
axisY <- c()
for(i in 1:length(p)){
  axisY[i] <- cosechaPromedio(p = p[i], N = 100, Nrep = 1000)
}
plot(p, axisY)

p[which.max(axisY)]










