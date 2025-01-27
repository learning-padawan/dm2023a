rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

instalar_paquete <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # Install the package if it's not installed
    install.packages(package_name)
  }
}

instalar_paquete("data.table")
instalar_paquete("rpart")
instalar_paquete("rpart.plot")

require("data.table")
require("rpart")
require("parallel")

print("Bibliotecas cargadas")

ksemillas <- c(185233, 185243, 185267, 185291, 185299, 185303, 185309, 185323, 185327, 185359, 185363, 185369, 185371, 185401, 185429, 185441, 185467, 185477, 185483, 185491) # reemplazar por las propias semillas

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla) # Cambiar por la primer semilla de cada uno !

  # genero el modelo
  modelo <- rpart("clase_ternaria ~ .", # quiero predecir clase_ternaria a partir del resto
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(list(
    "testing" = dataset[fold == 2, .N],
    "testing_pos" = dataset[fold == 2 & clase_ternaria == "BAJA+2", .N],
    "envios" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025)],
    "aciertos" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025 & clase_ternaria == "BAJA+2")],
    "ganancia_test" = ganancia_test_normalizada
  ))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("/Users/pablonavarro/Documents/ITBA/mcd/materias/2023.1.cuatrimestre/mcd_data_mining/mcd_dm/mcd_dm_trabajos") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


param_basicos <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 900, # minima cantidad de registros en un nodo para hacer el split
  "minbucket" = 440, # minima cantidad de registros en una hoja
  "maxdepth" = 5
) # profundidad máxima del arbol

# Un solo llamado, con la semilla 17
ArbolEstimarGanancia(17, param_basicos)


# la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
salidas <- mcmapply(ArbolEstimarGanancia,
  ksemillas, # paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
  MoreArgs = list(param_basicos), # aqui paso el segundo parametro
  SIMPLIFY = FALSE,
  mc.cores = 1
) # se puede subir a 5 si posee Linux o Mac OS

# muestro la lista de las salidas en testing para la particion realizada con cada semilla
salidas

# paso la lista a vector
tb_salida <- rbindlist(salidas)

tb_salida

# finalmente calculo la media (promedio)  de las ganancias
print(tb_salida[, mean(ganancia_test)])

# calculo todos los promedios
print(tb_salida[, lapply(.SD, mean)])



# desvio estandar Distribucion Binomial   sqrt( n * p * (1-p) )
