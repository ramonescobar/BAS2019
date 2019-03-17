# ID: Identifier
# Age: Customer's age in completed years
# Experience: Number of years of professional experience
# Income: Annual income of the customer ($000s)
# ZIP.Code: ZIP Code
# Family: Size Family size of the customer
# CCAvg: Average spending on credit cards per month ($000s)
# Education*: Education Level. 1: Undergrad; 2: Graduate; 3: Advanced/Professional
# Mortgage: Value of house mortgage if any ($000s)
# Securities*: Account Coded as 1 if customer has securities account with bank
# CD Account*: Coded as 1 if customer has certificate of deposit (CD) account with bank
# Online*: Banking Coded as 1 if customer uses Internet banking facilities
# Credit Card*: Coded as 1 if customer uses credit card issued by Universal Bank

library(corrplot)
library(RColorBrewer)
library(forecast)
library(gains)
library(caret)
library(ROCR)

######## 0 - Cargando archivos ########
bank.df <- read.csv(file.choose())

######## 1 - Exploracion de datos ########
head(bank.df)
ncol(bank.df)
nrow(bank.df)
str(bank.df)

# Busquemos nulos
sapply(bank.df, function(x) round(sum(is.na(x))/length(x)*100,4))
# Para variables numericas, podemos utilizar summary()
summary(bank.df)

# Agregar factor a variables categoricas
bank.df$Education <- factor(bank.df$Education, 
                            levels = c(1, 2, 3),
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))
bank.df$Securities.Account <- as.factor(bank.df$Securities.Account)
bank.df$CD.Account <- as.factor(bank.df$CD.Account)
bank.df$Online <- as.factor(bank.df$Online)
bank.df$CreditCard <- as.factor(bank.df$CreditCard)
bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)

# Para variables categoricas, podemos utilizar table()
table(bank.df$Education)
table(bank.df$Education)/nrow(bank.df)*100

# Creamos un bucle que nos permita obtner tablas cruzadas
factorvar <- sapply(bank.df, is.factor)

for (i in 1:length(factorvar)){
  if(factorvar[i]==TRUE){
    cat(paste("
              Table for ", names(factorvar[i]), sep=""))
    print(table(bank.df[,i]))
    print(table(bank.df[,i])/nrow(bank.df)*100)
    }
}

# Analisis de correlaciones
mat_cor <- cor(x= bank.df[,sapply(bank.df, is.numeric)])

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

# Podemos hacer un agrupamiento y revisar promedios por grupo
numericvar <- sapply(bank.df, is.numeric)

for (i in 1:length(numericvar)){
  if(numericvar[i]==TRUE){
    print(paste("Aggregate for ", names(numericvar[i]), sep=""))
    print(aggregate(bank.df[,i], by=list(bank.df$Personal.Loan), FUN=mean))
  }
}

# Ahora tablas cruzadas
factorvar <- sapply(bank.df, is.factor)
for (i in 1:length(factorvar)){
  if(factorvar[i]==TRUE){
    print(table(bank.df$Personal.Loan, bank.df[,i], dnn=c("Loan", names(factorvar[i]))))
    print(prop.table(table(bank.df$Personal.Loan, bank.df[,i], dnn=c("Loan", names(factorvar[i]))),1)*100)
  }
}

######## 3 - Creando datasets ########
# Borrando columnas que no se utilizaran
bank.df <- bank.df[ , -c(1, 5)]
bank.df$Personal.Loan <- as.numeric(as.character(bank.df$Personal.Loan))

# Creando particiones
set.seed(2)
train.index <- sample(x= c(1:nrow(bank.df)), 
                      size= nrow(bank.df)*0.6)

train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

######## 4 - Haciendo modelo ########
# crear regresion logistica
# Utiliza glm() (general linear model) con family = "binomial" para crear una RL
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)

#Revisando odds
round(data.frame(summary(logit.reg)$coefficients, odds = exp(coef(logit.reg))), 5)

#Cuales son las variables mas importantes
importance <- varImp(logit.reg)

######## 5 - Revisando predicciones ########
# Hacer predicciones
logit.reg.pred <- predict(object=logit.reg, 
                          valid.df, type = "response")

# Para calcular el r2 
basereg <- glm(Personal.Loan ~ 1, data = train.df, family = "binomial")
summary(basereg)
1-(logLik(logit.reg))/(logLik(basereg))

######## 6 - Curva ROC ########
# Roc Curve
pred <- prediction(predictions=logit.reg.pred, 
                   labels = valid.df$Personal.Loan)

perf <- performance(pred, 
                    measure = "tpr", 
                    x.measure = "fpr") 

par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf, col=rainbow(10))
abline(a=0, b= 1)

#Calculando Area under curve
auc <- round(as.numeric(performance(pred,"auc")@y.values),2)
auc_value <- paste("AUC= ", auc, sep="")
legend(0.7,0.2,auc_value,border="white",cex=1,box.col = "white")
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

######## 7 - Encontrando mejor cutoff value ########
# Creando funcion
pred <- prediction(logit.reg.pred, valid.df$Personal.Loan)
perf <- performance(pred, x.measure = "prec", measure = "rec")

#Maximiza el F1 Score en pred
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (2*(x*y))/(x+y)
    ind = min(which(d == max(d, na.rm=TRUE)))
    c(recall = y[[ind]], precision = x[[ind]], 
      cutoff = p[[ind]], f1score= max(d, na.rm=TRUE))
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

#Listar resultados
print(opt.cut(perf, pred))

cutoff <- opt.cut(perf, pred)[3]

#Graficando Precision y Recall
prec_rec <- performance(pred, measure="prec", x.measure="rec") 
plot(prec_rec, col="red")
lines(y= unlist(prec_rec@alpha.values),
      x= unlist(prec_rec@x.values), col= "green")
legend(x= 0.05,
       y= 0.4,
       legend= c("x=Rec/y=Pre","x=Rec/y=Cutoff"),
       col= c("red","green"),
       lty= 1:2,
       border="white",
       cex=1,
       box.col = "white")
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

# Graficando Precision y Cutoff
gr1 <- performance(pred, measure="prec", x.measure="cutoff") 
plot(gr1, col=rainbow(10))
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

# Graficando Recall y Cutoff
gr2 <- performance(pred, measure="rec", x.measure="cutoff") 
plot(gr2, col=rainbow(10))
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

# Graficando Recall/Precicion y Cutoff
plot(gr1, col="red")
lines(y= unlist(gr2@y.values),
      x= unlist(gr2@x.values), col="blue")
legend(x= 0.4,
       y= 0.4,
       legend= c("Precision","Recall"),
       col= c("red","blue"),
       lty= 1:2,
       border="white",
       cex=1,
       box.col = "white")
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

# Graficando Accuracy y Cutoff
gr3 <- performance(pred, measure="acc", x.measure="cutoff") 
plot(gr3, col=rainbow(10))
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")

######## 8 - Matriz de confusion ######## 
# Calculando matriz de confusion version 1
table(real= as.factor(valid.df$Personal.Loan),
      predicted= as.factor(ifelse(logit.reg.pred > cutoff, 1, 0)))

# Para calcular Recall
matrec <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > cutoff, 1, 0))),1)*100
matrec[4]

# Para calcular Precision
matpre <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > cutoff, 1, 0))),2)*100
matpre[4]

# Para calcular Accuracy
mat_acc <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > cutoff, 1, 0))))*100

mat_acc[1]+mat_acc[4]

# MCR
100-(mat_acc[1]+mat_acc[4])


# Calculando matriz de confusion version 2
CM <- confusionMatrix(data= as.factor(ifelse(logit.reg.pred > cutoff, 1, 0)), 
                      reference= as.factor(valid.df$Personal.Loan),
                      positive= "1",
                      mode= "prec_recall")

# Matriz de confusion
t(CM$table)

# Para calcular Accuracy
options(scipen=999)
CM$overall

# Para calcular Recall y Precision
options(scipen=999)
CM$byClass

######## 8 - Lift Chart ######## 
# Lift Chart
gain <- gains(actual= valid.df$Personal.Loan, 
              predicted= logit.reg.pred,
              groups= 10)

par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(x= c(0,gain$depth),xlab="% cases", 
     y= c(0,gain$cume.pct.of.total*100),ylab="Lift",
     type="l", col="red")
abline(a=0, b= 1)
text(x= gain$depth,xlab="% cases", 
     y= gain$cume.pct.of.total*100,
     labels= round(gain$cume.pct.of.total*100,0),
     pos=3, cex=0.7, offset=-0.5)
text(x=c(1:10)*10,
     y=c(1:10)*10,
     labels= c(1:10)*10,
     pos=2, cex=0.7, offset=0)
grid(nx = 10, ny = NULL, col = "lightgray", lty = "dotted")
