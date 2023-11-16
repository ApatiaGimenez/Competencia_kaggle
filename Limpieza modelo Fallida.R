library(dplyr)
library(ggplot2)
library(tidyverse)

Train <- as.data.frame(origen)
Test <- as.data.frame(testear)


#Uno los datasets para hacer el frecuency encoding
Train$dataset <- "Train"
Test$dataset <- "Test"
Test$averageRating <- NA

datos <- rbind(Train, Test)

#frecuency encoding

#titleType

# Calcular la frecuencia de cada categoría
frecuencia <- table(datos$titleType)

# Calcular el porcentaje de veces que aparece cada categoría
porcentaje <- prop.table(frecuencia)

# Crear un nuevo data frame con el encoding de frecuencia
encoding_df <- data.frame(
  titleType = names(porcentaje),
  Tipo = as.vector(porcentaje)
)

# Realizar la unión con el data frame original
datos <- merge(datos, encoding_df, by = "titleType", all.x = TRUE)

# Verificar el resultado
View(datos)


#genres_x


# Calcular la frecuencia de cada categoría
frecuencia <- table(datos$genres_x)

# Calcular el porcentaje de veces que aparece cada categoría
porcentaje <- prop.table(frecuencia)

# Crear un nuevo data frame con el encoding de frecuencia
encoding_dfI <- data.frame(
  genres_x = names(porcentaje),
  Genero = as.vector(porcentaje)
)

# Realizar la unión con el data frame original
datos <- merge(datos, encoding_dfI, by = "genres_x", all.x = TRUE)

# Verificar el resultado
View(datos)



#directors


# Calcular la frecuencia de cada categoría
frecuencia <- table(datos$directors)

# Calcular el porcentaje de veces que aparece cada categoría
porcentaje <- prop.table(frecuencia)

# Crear un nuevo data frame con el encoding de frecuencia
encoding_dfI <- data.frame(
  directors = names(porcentaje),
  Directores = as.vector(porcentaje)
)

# Realizar la unión con el data frame original
datos <- merge(datos, encoding_dfI, by = "directors", all.x = TRUE)

# Verificar el resultado
View(datos)


#writers

# Calcular la frecuencia de cada categoría
frecuencia <- table(datos$writers)

# Calcular el porcentaje de veces que aparece cada categoría
porcentaje <- prop.table(frecuencia)

# Crear un nuevo data frame con el encoding de frecuencia
encoding_dfI <- data.frame(
  writers = names(porcentaje),
  Escritores = as.vector(porcentaje)
)

# Realizar la unión con el data frame original
datos <- merge(datos, encoding_dfI, by = "writers", all.x = TRUE)

# Verificar el resultado
View(datos)




#Separo nuevamente los data sets
train_data <- datos[datos$dataset == "Train", ]
test_data <- datos[datos$dataset == "Test", ]



train_data <- train_data[, c("averageRating","numVotes","Tipo", "isAdult", "startYear", "endYear", "runtimeMinutes", "Genero","Directores","Escritores")]
View(train_data)

test_data <- test_data[, c("numVotes","Tipo", "isAdult", "startYear", "endYear", "runtimeMinutes", "Genero","Directores","Escritores")]
View(test_data)


setwd("C:/Users/Win/Desktop/Kaggle")
write.table(train_data, file= "train.csv", sep=",", row.names = F)
write.table(test_data, file= "test.csv", sep=",", row.names = F)