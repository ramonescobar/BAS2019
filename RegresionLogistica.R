# ID: Identifier
# Age: Customer???s age in completed years
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
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))
bank.df$Securities.Account <- as.factor(bank.df$Securities.Account)
bank.df$CD.Account <- as.factor(bank.df$CD.Account)
bank.df$Online <- as.factor(bank.df$Online)
bank.df$CreditCard <- as.factor(bank.df$CreditCard)
bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)

# Para variables categoricas, podemos utilizar table()
table(bank.df$Education)
table(bank.df$Education)/nrow(bank.df)*100

# Variablas tablas de una sola vez, las variables deben de ser factor para funcionar
factorvar <- sapply(bank.df, is.factor)
for (i in 1:length(factorvar))
{
  if(factorvar[i]==T)
  {
    cat(paste("
              Table for ", names(factorvar[i]), sep=""))
    print(table(bank.df[,i]))
    print(table(bank.df[,i])/nrow(bank.df)*100)
    }
}

# Analisis de correlaciones
mat_cor <- cor(x= bank.df [,sapply(bank.df, is.numeric)])

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
for (i in 1:length(numericvar))
{
  if(numericvar[i]==T)
  {
    print(paste("Aggregate for ", names(numericvar[i]), sep=""))
    print(aggregate(bank.df[,i], by=list(bank.df$Personal.Loan), FUN=mean))
  }
}

# Ahora tablas cruzadas
factorvar <- sapply(bank.df, is.factor)
for (i in 1:length(factorvar))
{
  if(factorvar[i]==T)
  {
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

######## 5 - Revisando predicciones ########
# Hacer predicciones
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Matriz de confusion
table(real= as.factor(valid.df$Personal.Loan),
  predicted= as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)))

# Para calcular recall
matrec <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > 0.5, 1, 0))),1)*100
matrec[4]

# Para calcular precision
matpre <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > 0.5, 1, 0))),2)*100
matpre[4]

# Para calcular Accuracy
mat_acc <- prop.table(table(real= as.factor(valid.df$Personal.Loan),
                 predicted= as.factor(ifelse(logit.reg.pred > 0.5, 1, 0))))*100

mat_acc[1]+mat_acc[4]

# MCR
100-(mat_acc[1]+mat_acc[4])


# Roc Curve
roc(valid.df$Personal.Loan, logit.reg.pred, direction="<")
plot(roc(valid.df$Personal.Loan, logit.reg.pred, direction="<"),
     col="blue", lwd=3)



# Para calcular el r2 
basereg <- glm(Personal.Loan ~ 1, data = train.df, family = "binomial")
1-(logLik(logit.reg))/(logLik(basereg))