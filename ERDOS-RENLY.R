#---------- ERDOS-RENLY MODEL -----------
#---------- REDES ALEATORIAS ------------

# Primero hice un ejercicio para ver como funcionaba y después abajo ya hice el bueno :)

# Función <- sample_gnp()
## Parametros: 
  # n(número de vértices)
  # p (probabilidad que exista enlace entre vértices "p" y "m")
  # Opcionales: directed, loops

library(igraph)
library(igraphdata)

r1 <- sample_gnp(n = 200, # Número de vertices
                       p = 0.01, # Probabilidad que se enlace
                       directed = FALSE, # Si es direccionada o no
                       loops = FALSE) # Si presenta loops

set.seed(1001)
plot(r1,
     vertex.label = NA,
     vertex.size = 4,
     vertex.color = "pink",
     eddge.color = "violet",
     edge.size = 0.5)



# -------- EJERCICIO ERDOS-RENLY MODEL -----------

set.seed(1001) # Para que sea reproducible

# Crear un object con la red aleatoria y replicar 100 veces
r2 <- replicate(n = 100, sample_gnp(n = 20, # Número de vertices
                                      p = 0.5, # Probabilidad que se enlace
                                      directed = FALSE, # Si es direccionada o no
                                      loops = FALSE), # Si presenta loops o no
                  simplify = FALSE) 


# Cálculo de degree a todos los elementos con sapply
r2.2 <- sapply(r2, FUN = degree) 


# Crear data frame 
DF_r2 <- data.frame(r2.2)



# Crear tabla y calcular mediana, max y min en cada columna
DF_r2_table <- data.frame(
  Max = sapply(DF_r2, FUN = max),
  Min = sapply(DF_r2, FUN = min),
  Median = sapply(DF_r2, FUN = median))
  

