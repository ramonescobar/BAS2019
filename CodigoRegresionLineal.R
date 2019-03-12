library(corrplot)
library(RColorBrewer)
library(forecast)

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

car.df$MetColor<-as.factor(car.df$MetColor)
car.df$Automatic<-as.factor(car.df$Automatic)

# Para variables categoricas, podemos utilizar table()
(table(car.df$FuelType))
#(table(car.df$Automatic/nrow(car.df))*100
#(table(car.df$MetColor)/nrow(car.df))*100

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
aggregate(car.df$Price, 
          by=list(car.df$FuelType), 
          FUN=mean)

aggregate(car.df$Price, 
          by=list(car.df$MetColor), 
          FUN=mean)

aggregate(car.df$Price, 
          by=list(car.df$Automatic), 
          FUN=mean)

######## 3 - Creando datasets ########
# Ocupando solamente las primeras 1K fias de
car.df <- car.df[1:1000, ]

# Haciendo las particiones
set.seed(1) # Esto se utiliza para efectos de reproduccion del ejercicio
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index,]
valid.df <- car.df[-train.index,]

######## 4 - Haciendo modelo ########
# Ocupa . despues ~ para incluir todas las columnas restantes como variables
car.lm <- lm(Price ~ ., data = train.df)

# Ocupar la siguiente sintaxis para que los valores se muestren con decimales
options(scipen = 999)
summary(car.lm)

######## 5 - Revisando predicciones ########
# Hacer predicciones
car.lm.pred <- predict(car.lm, valid.df)

# Ocupa accuracy() para crear metricas de exactitud predictiva 
accuracy(car.lm.pred, 
         valid.df$Price)

# Graficos para evaluar residuos
all.residuals <- valid.df$Price - car.lm.pred

# Histograma de residuos
hist(all.residuals, breaks 
     = 25, xlab = "Residuals", main = "")

layout(matrix(c(1,2,3,4),2,2)) # Codigo opcional 
plot(car.lm)

######## 6 - Encontrando nuevos modelos ########
# Utilizar el parametro directions con los valores "backward", "forward", or "both"
car.lm.step <- step(car.lm, 
                    direction = "both")
summary(car.lm.step)

car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

save(car.lm, file="C:/Users/Ramon/Dropbox/Quinto año/BAS/modelo.rda")
