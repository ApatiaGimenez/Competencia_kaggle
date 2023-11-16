library(dplyr)
library(ggplot2)
library(tidyverse)






crudo <- as.data.frame(origen)
crudo2 <- as.data.frame(testear)
View(crudo)
data <- crudo[, c("averageRating","numVotes","titleType", "isAdult", "startYear", "endYear", "runtimeMinutes", "genres_x","directors","writers")]
data2 <- crudo2[, c("numVotes","titleType", "isAdult", "startYear", "endYear", "runtimeMinutes", "genres_x","directors","writers")]

#frecuency encoding

#titleType

frec_titleType <- table(data$titleType)
data$frec_titleType <- frec_titleType[data$titleType] / sum(frec_titleType)
data2$frec_titleType <- frec_titleType[data2$titleType] / sum(frec_titleType) #Agrego estos datos en dataset de testear
data <- data %>% select(-titleType)
data2 <- data2 %>% select(-titleType)
#genres_x

#Hago frecuency encoder para genre_x y cuento la cantidad de generos que tiene 1 peli
frec_genres_x <- table(data$genres_x)
data$frec_genres_x <- frec_genres_x[data$genres_x] / sum(frec_genres_x)
data$cantidad_generos <- str_count(data$genres_x, ',') + 1
data2$cantidad_generos <- str_count(data2$genres_x, ",") + 1

m7 <- mean(data$frec_genres_x, na.rm = TRUE)
data$frec_genres_x <- ifelse(is.na(data$frec_genres_x), m7, data$frec_genres_x)
data2$frec_genres_x <- frec_genres_x[data2$genres_x] / sum(frec_genres_x) #Agrego estos datos en dataset de testear
data2$frec_genres_x <- ifelse(is.na(data2$frec_genres_x), m7, data2$frec_genres_x)

data <- data %>% select(-genres_x)
data2 <- data2 %>% select(-genres_x)




#directors y writers

data$cantidad_writers <- str_count(data$writers, ',') + 1
data$cantidad_directors <- str_count(data$directors, ',') + 1
data2$cantidad_writers <- str_count(data2$writers, ',') + 1
data2$cantidad_directors <- str_count(data2$directors, ',') + 1

frec_directors <- table(data$directors)
data$frec_directors <- frec_directors[data$directors] / sum(frec_directors)
data2$frec_directors <- frec_directors[data2$directors] / sum(frec_directors)#Agrego estos datos en dataset de testear
m6 <- mean(data$frec_directors)
data2$frec_directors <- ifelse(is.na(data2$frec_directors), m6, data2$frec_directors)

frec_writers <- table(data$writers)
data$frec_writers <- frec_writers[data$writers] / sum(frec_writers)
data2$frec_writers <- frec_writers[data2$writers] / sum(frec_writers)#Agrego estos datos en dataset de testear
m10 <- mean(data$frec_writers)
data2$frec_writers <- ifelse(is.na(data2$frec_writers), m10, data2$frec_writers)

data <- data %>% select(-directors, -writers)
data2 <- data2 %>% select(-directors, -writers)

data <- data %>%
  mutate(tiempoEmision = ifelse(endYear == 0 | endYear < startYear, 0, endYear - startYear))

data2 <- data2 %>%
  mutate(tiempoEmision = ifelse(endYear == 0 | endYear < startYear, 0, endYear - startYear))

View(data)
View(data2)


setwd("C:/Users/Win/Desktop/Kaggle")
write.table(data, file= "train.csv", sep=",", row.names = F)
write.table(data2, file= "test.csv", sep=",", row.names = F)


matriz <- cor(data)


print(matriz)