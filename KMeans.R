library(corrplot)
library(RColorBrewer)
library(forecast)
library(dplyr)

######## 0 - Cargando archivos ########
car.df <- read.csv(file.choose())

######## 1 - Exploracion de datos ########
head(car.df)
ncol(car.df)
nrow(car.df)
str(car.df)

# Busquemos nulos
sapply(car.df, function(x) round(sum(is.na(x))/length(x)*100,4))

# Para variables numericas, podemos utilizar summary()
summary(car.df)

# Agregar factor a 2 variables

# Para variables categoricas, podemos utilizar table()
table(car.df$FuelType)
table(car.df$FuelType)/nrow(car.df)

# Analisis de correlaciones
mat_cor <- cor(x= car.df [,sapply(car.df, is.numeric)])

corrplot(mat_cor,
         type="upper",
         order="hclust",
         method = "circle", 
         col=brewer.pal(n=8, name="RdYlBu"))

corrplot(mat_cor,
         type="upper",
         order="hclust",
         method = "number", 
         col=brewer.pal(n=8, name="RdYlBu"))

# Para una variable categorica no se puede realizar un analisis de correlaciones
# Podemos hacer un agrupamiento y revisar promedios por grupo
aggregate(car.df$Price, by=list(car.df$FuelType), FUN=mean)

######## 3 - Modificando variables de factor ########

car.df$MetColor <- as.factor(car.df$MetColor)
car.df$Automatic <- as.factor(car.df$Automatic)

######## 4 - Creando nuevo dataset ########

# Seleccionando solamente variables numericas
data <- car.df [,sapply(car.df, is.numeric)]

######## 5 - Normalizando el dataset ########
data_norm <- as.data.frame(unlist(sapply(data, scale)))

######## 6 - Creando segmentacion con 6 clusters ########
km <- kmeans(x= data_norm, 
             centers= 6)

######## 7 - Revisando resultados ########

# Asignando valores a dataset original
data$cluster <- km$cluster

# Dando significado a segmentos
aggregate(data, by=list(data$cluster), FUN=mean)

# Crear un grafico vacio
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, ncol(data)))
# Nombres a los ejes
axis(1, at = c(1:ncol(data)), labels = names(data))
# Graficar centroides
for (i in c(1:max(km$cluster))){
  lines(km$centers[i,],
        lty= i, 
        lwd= 2,
        col= ifelse(i %in% c(1, 3, 5),
                    "black", "dark grey")
  )
}
# Darle nombre a los cluster
for (i in c(1:ncol(data)))
{
  text(x = i, 
       y = km$centers[, i],
       labels = paste("C", c(1:max(km$cluster)), sep=""),
       pos=2, cex=0.7
  )
}


######## 8 - Iterando clusters ########
set.seed(123)
j <- 10

models <- data.frame(k=integer(),
                     dentro=numeric(),
                     fuera=numeric(),
                     total=numeric(),
                     rcuadrado=numeric()
                     )

for (k in 1:j)
{
  
  output <- kmeans(x= data_norm,
                   centers= k)
  
  print(round((output$betweenss/output$totss)*100,1))
  
  # Agregando cluster a datasets
  var_name <- paste("cluster", k, sep="_")
  data[,var_name] <- output$cluster
  data_norm[,var_name] <- output$cluster
  
  # Collect model information
  models[k,("k")] <- k
  models[k,("dentro")] <- output$tot.withinss
  models[k,("fuera")] <- output$betweenss
  models[k,("total")] <- output$totss
  models[k,("rcuadrado")] <- round(output$betweenss/output$totss, 3)
  
  ##Creating Cluster Center Tables and Graphs 
  title <- paste("k-means Solution with", k, "Cluster", sep=" ")
  print(title)
  
  plot(y= models$dentro,
       x= models$k,
       type="b")
  
  plot(y= models$rcuadrado,
       x= models$k,
       type="b")
}

aggregate(data, by=list(data$cluster_6), FUN=mean)
