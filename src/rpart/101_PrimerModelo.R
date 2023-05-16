instalar_paquete <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # Install the package if it's not installed
    install.packages(package_name)
  }
}

# Arbol elemental con libreria  rpart
main <- function() {
  primer_arbol(trial_number = '_ConHiperparametrosDeBayesianoBO')
}

primer_arbol <- function(trial_number) {
  # Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
  instalar_paquete("data.table")
  instalar_paquete("rpart")
  instalar_paquete("rpart.plot")

  # cargo las librerias que necesito
  require("data.table")
  require("rpart")
  require("rpart.plot")

  # Aqui se debe poner la carpeta de la materia de SU computadora local
  setwd("/Users/pablonavarro/Documents/ITBA/mcd/materias/2023.1.cuatrimestre/mcd_data_mining/mcd_dm/mcd_dm_trabajos") # Establezco el Working Directory

  # cargo el dataset
  dataset <- fread("./datasets/dataset_pequeno.csv")

  dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
  dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

  # genero el modelo,  aqui se construye el arbol
  # rpart llama al algo que construye el arbol de decisiones
  # toma el data set, le pasa los parametros y genera un arbol de decisión
  modelo <- rpart(
    formula = "clase_ternaria ~ .", # quiero predecir clase_ternaria a partir de el resto de las variables
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.624159443588753, # esto significa no limitar la complejidad de los splits
    minsplit = 663.746805990154, # minima cantidad de registros para que se haga el split
    minbucket = 322.739030387551, # tamaño minimo de una hoja que uno acepta
    maxdepth = 8
  ) # profundidad maxima del arbol


  # grafico el arbol
  prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0)


  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )

  # prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades

  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

  # solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

  # genero el archivo para Kaggle
  # primero creo la carpeta donde va el experimento
  dir.create("./exp/")
  dir.create("./exp/KA2001")

  
  # escribo el archivo para Kaggle
  # create the filename incliding the trial number
  filename <- paste0("./exp/KA2001/K101_", trial_number, ".csv")
  fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
    file = filename, # Cambiar manualmente los ultimos 3 numeros del csv para cada corrida que subo a Kaggle
    sep = ","
  )
}
