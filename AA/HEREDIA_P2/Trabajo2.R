###############################################
#TRABAJO 2   Aprendizaje Automático           #
# Mª Cristina Heredia Gómez                   #
#3º de Ing.Informática - Univerdad de Granada #
###############################################

#ejercicio1 ->Usar la base de datos Weekly (ISLR)
library(ISLR)
help(Weekly)
attach(Weekly)
#apartado 1 -> 
#matriz de correlaciones
cor(Weekly[ ,-9])
#gráfica de volume
plot(Volume, pch=20, col='navy')

#apartado 2 ->
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family=binomial)
summary(glm.fit)

#apartado 3 -> en primer lugar, dados los valores de los predictores, calculamos la
#probabilidad de que el mercado suba:
glm.probs=predict(glm.fit, type='response',data=Weekly)
glm.probs[1:10]
#con la función constrast R crea una variable con 1 para Up. 
contrasts(Direction)
#creamos vector con las probabilidads a "Down" o "Up"
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
#creamos matriz de confusión
table(glm.pred,Direction)
#hacemos la cuenta para ver cuantas son correctas
(54+557)/1089
#hacemos la cuenta para obtener el  error
(430+48)/1089

#apartado 4 -> 
#primero obtenemos subconjunto de datos entre 1990 y 2008 y hacemos la regresión logística
#usando Lag2 como único predictor:
sub_set=(Year<2009)
glm.fit2=glm(Direction~Lag2, data=Weekly,family=binomial,subset=sub_set)
summary(glm.fit2)

Weekly.2008=Weekly[!sub_set,]
dim(Weekly.2008)
Direction.2008=Direction[!sub_set]
#probabilidad de que el mercado suba:
glm.probs2=predict(glm.fit2,Weekly.2008, type='response')
#con la función constrast R crea una variable con 1 para Up. 
contrasts(Direction)
#creamos vector con las probabilidads a "Down" o "Up"
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>0.5]="Up"
#creamos matriz de confusión
table(glm.pred2,Direction.2008)
#hacemos la cuenta para ver porcentaje de aciertos
(9+56)/104
#porcentaje de acertar con el sí(mercado crece):
(56/(56+34))
#porcentaje de acertar con el no(mercado cae):
(9/(9+5))

#apartado 5 ->
#LDA: 
#cargamos la libreria MASS que contiene la función que vamos a usar:
library(MASS)
#ajustamos lda con datos trainning:
lda.fit =lda( Direction~Lag2 , data=Weekly, subset=sub_set)
#predecimos usando también los datos test
lda.pred = predict( lda.fit, Weekly.2008)
lda.class = lda.pred$class
#creamos matriz de confusión:
table(lda.class , Direction.2008)

#QDA:
#ajustamos modelo  usando datos trainning:
qda.fit = qda (Direction~Lag2 , data =Weekly,  subset = sub_set)
#predecidmos usando modelo y datos test:
qda.class = predict(qda.fit , Weekly.2008)$class
#creamos matriz:
table(qda.class, Direction.2008)

#KNN con k=10:
#cargamos libreria
library ( class )
#inicializamos semilla aleatoria
set . seed (1) 
train.X = cbind ( Lag2, Lag2 ) [ sub_set ,]
test.X = cbind ( Lag2, Lag2 ) [! sub_set ,]
train.Direction = Direction [ sub_set ]
#ajustamos modelo knn:
knn.pred = knn ( train.X , test.X , train.Direction , k =1)
#creamos matriz de confusión:
table ( knn.pred , Direction.2008)

#apartado 6 ->
cor(Weekly[ ,-9])
#RLG:
glm.fit2=glm(Direction~Lag2+Lag3, data=Weekly,family=binomial,subset=sub_set)
glm.probs2=predict(glm.fit2,Weekly.2008, type='response')
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2,Direction.2008)

glm.fit2=glm(Direction~Lag2*Lag3+Lag4, data=Weekly,family=binomial,subset=sub_set)
glm.probs2=predict(glm.fit2,Weekly.2008, type='response')
glm.pred2=rep("Down",104)
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2,Direction.2008)


#LDA:
#cargamos la libreria MASS que contiene la función que vamos a usar:
library(MASS)

lda.fit =lda( Direction~Lag2*Lag3+Lag4 , data=Weekly, subset=sub_set)
lda.pred = predict( lda.fit, Weekly.2008)
lda.class = lda.pred$class
#creamos matriz de confusión:
table(lda.class , Direction.2008)

#QDA:

qda.fit=qda(Direction~Lag2*Lag3+Lag4, data =Weekly,subset = sub_set)
#predecidmos usando modelo y datos test:
qda.class=predict(qda.fit , Weekly.2008)$class
#creamos matriz:
table(qda.class, Direction.2008)


#ajustamos modelo  usando datos trainning:
qda.fit=qda(Direction~Lag2+Lag3 , data =Weekly,subset = sub_set)
#predecidmos usando modelo y datos test:
qda.class=predict(qda.fit , Weekly.2008)$class
#creamos matriz:
table(qda.class, Direction.2008)

#KNN:
#cargamos libreria
library ( class )
#inicializamos semilla aleatoria
set.seed(1) 
train.X = cbind ( Lag2, Lag3 ) [ sub_set ,]
test.X = cbind ( Lag2, Lag3 ) [! sub_set ,]
train.Direction = Direction [ sub_set ]
#ajustamos modelo knn:
knn.pred = knn ( train.X , test.X , train.Direction , k =10)
#creamos matriz de confusión:
table ( knn.pred , Direction.2008)

#inicializamos semilla aleatoria
set.seed(1) 
train.X = cbind ( Lag2, Lag3 ) [ sub_set ,]
test.X = cbind ( Lag2, Lag3 ) [! sub_set ,]
train.Direction = Direction [ sub_set ]
#ajustamos modelo knn:
knn.pred = knn ( train.X , test.X , train.Direction , k =12)
#creamos matriz de confusión:
table ( knn.pred , Direction.2008)

#aparado 7 -> validación cruzada con 10 particiones:
#inicializamos la semilla:
install.packages('boot')
library(boot)
library(class)

set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(Direction~poly(Lag2*Lag3+Lag4,i),data=Weekly,family=binomial)
  cv.error.10[i]=cv.glm(Weekly,glm.fit,K=10)$delta[1]
}
cv.error.10

#Ejercicio2->En este ejercicio desarrollaremos un modelo para predecir si un coche tiene un
#consumo de carburante alto o bajo usando la base de datos Auto.

#apartado1->
#Crear una variable binaria, mpg01, que será igual 1 si mpg contiene un valor
#por encima de la mediana , y 0 si mpg contiene un valor por debajo de la
#mediana.

#cargamos librería
library(ISLR)
attach(Auto)
#vemos número de filas para saber cuántos elementos tiene el conjunto de datos
nrow(Auto)
#creamos matriz binaria mpg01:
mpg01=rep(0,nrow(Auto))
mpg01[mpg > median(mpg)]=1
mpg01
# unimos en un mismo conjunto de datos mpg01 y las otras variables de Auto:
Auto=data.frame(Auto,mpg01)

#apartado2->Explorar los datos gráficamente para investigar la asociación entre mpg01 y las
#otras características.
library(ISLR)
attach(Auto)
pairs(Auto)

#boxplot cylinders vs mpg01:
boxplot(cylinders~mpg01,data=Auto, main="cylinders vs mpg01",xlab="mpg01", ylab="cylinders", col=(c("gold","orange")))
#boxplot displacement vs mpg01:
boxplot(displacement~mpg01,data=Auto, main="displacement vs mpg01",xlab="mpg01", ylab="displacement", col=(c("gold","orange"))) 
#boxplot horsepower vs mpg01:
boxplot(horsepower~mpg01,data=Auto, main="horsepower vs mpg01",xlab="mpg01", ylab="horsepower", col=(c("gold","royalblue"))) 
#boxplot weight vs mpg01:
boxplot(weight~mpg01,data=Auto, main="weight vs mpg01",xlab="mpg01", ylab="weight", col=(c("gold","palegreen3")))
#boxplot acceleration vs mpg01:
boxplot(acceleration~mpg01,data=Auto, main="acceleration vs mpg01",xlab="mpg01", ylab="acceleration", col=(c("gold","palevioletred")))
#boxplot year vs mpg01:
boxplot(year~mpg01,data=Auto, main="year vs mpg01",xlab="mpg01", ylab="year", col=(c("gold","olivedrab1")))
#boxplot origin vs mpg01:
boxplot(origin~mpg01,data=Auto, main="origin vs mpg01",xlab="mpg01", ylab="origin", col=(c("gold","navajowhite1")))

#Apartado3->Definir un conjunto de validación dividiendo los datos en un conjunto de
#entrenamiento 70% y otro de test 30%:
set.seed(1)
#calculo 70% :
entrenamiento=nrow(Auto)*0.7
#uso la orden sample para crear un conjunto de trainning com el 70% de los datos:
train=sample(392,entrenamiento)
Auto.train=Auto[train, ]
Auto.test=Auto[-train, ]
#comprobamos que tenemos los porcentajes dseados de datos trainning y datos test:
dim(Auto.train)
dim(Auto.test)

#apartado 2.3.a-> AJUSTE LDA
lda.fit =lda( mpg01~cylinders+weight+displacement+horsepower, data=Auto, subset=train)
lda.pred = predict( lda.fit, Auto.test)
lda.class = lda.pred$class
length(lda.class)
mpg01.test=mpg01[-train]
length(mpg01.test)
table(lda.class , mpg01.test)
#calculo error: (1-acierto)
1-0.8728814

#apartado 2.3.b-> AJUSTE QDA

qda.fit=qda(mpg01~cylinders+weight+displacement+horsepower, data =Auto,subset=train)
#predecidmos usando modelo y datos test:
qda.class=predict(qda.fit ,Auto.test)$class
#creamos matriz:
table(qda.class,mpg01.test)

#apartado 2.3.c-> AJUSTE RLG
glm.fit2=glm(mpg01~cylinders+weight+displacement+horsepower, data =Auto,family=binomial,subset=train)
summary(glm.fit2)
#probabilidad de que el mercado suba:
glm.probs2=predict(glm.fit2,Auto.test, type='response')
#creamos vector con las probabilidads a "Down" o "Up"
glm.pred2=rep("Down",118)
glm.pred2[glm.probs2>0.5]="Up"
#creamos matriz de confusión
table(glm.pred2,mpg01.test)

#apartado 2.3.d-> AJUSTE KNN
########
###     K=1   #### 
library ( class )

#inicializamos semilla aleatoria
set.seed(1) 
train.X = cbind( cylinders,weight,displacement,horsepower )[ train ,]
test.X = cbind( cylinders,weight,displacement,horsepower )[-train ,]
train.mpg01 = mpg01[train]
mpg01.test=mpg01[-train]
#ajustamos modelo knn:
knn.pred = knn(train.X , test.X , train.mpg01 , k =1)
#creamos matriz de confusión:
table ( knn.pred , mpg01.test)
#cuenta para calcular aciertos:
(53+49)/118
#cuenta para calcular error:
1-0.8644068

###     K=3   #### 
library ( class )

#inicializamos semilla aleatoria
set.seed(1) 
train.X = cbind( cylinders,weight,displacement,horsepower )[ train ,]
test.X = cbind( cylinders,weight,displacement,horsepower )[-train ,]
train.mpg01 = mpg01[train]
mpg01.test=mpg01[-train]
#ajustamos modelo knn:
knn.pred = knn(train.X , test.X , train.mpg01 , k =3)
#creamos matriz de confusión:
table ( knn.pred , mpg01.test)
#cuenta para calcular aciertos:
(53+53)/118
#cuenta para calcular error:
1-0.8983051

###     K=10   #### 
set.seed(1) 
train.X = cbind( cylinders,weight,displacement,horsepower )[ train ,]
test.X = cbind( cylinders,weight,displacement,horsepower )[-train ,]
train.mpg01 = mpg01[train]
mpg01.test=mpg01[-train]
#ajustamos modelo knn:
knn.pred = knn(train.X , test.X , train.mpg01 , k =10)
#creamos matriz de confusión:
table ( knn.pred , mpg01.test)
#cuenta para calcular aciertos:
(51+53)/118
#cuenta para calcular error:
1-0.8813559

###     K=100   #### 
set.seed(1) 
train.X = cbind( cylinders,weight,displacement,horsepower )[ train ,]
test.X = cbind( cylinders,weight,displacement,horsepower )[-train ,]
train.mpg01 = mpg01[train]
mpg01.test=mpg01[-train]
#ajustamos modelo knn:
knn.pred = knn(train.X , test.X , train.mpg01 , k =100)
#creamos matriz de confusión:
table ( knn.pred , mpg01.test)
#cuenta para calcular aciertos:
(54+57)/118
#cuenta para calcular error:
1- 0.940678

#Apartado 2.4->Repetir los experimentos del punto anterior pero usando Validación Cruzada de 5-particiones
library(boot)
library(class)

set.seed(17)
cv.error=rep(0,4)
for(i in 1:4){
  glm.fit=glm(mpg01~poly(cylinders+weight+displacement+horsepower,i),data=Auto)
#  lda.fit =lda( mpg01~poly(cylinders+weight+displacement+horsepower,i), data=Auto,CV=TRUE)
  cv.error[i]=cv.glm(Auto,glm.fit,K=5)$delta[1]
}
cv.error

lda.fit =lda( mpg01~poly(cylinders+weight+displacement+horsepower), data=Auto,CV=TRUE)
cv.error.5=cv.glm(Auto,glm.fit,K=5)$delta[1]

#para el apartado A) LDA:

mpg01=rep(0,nrow(Auto))
mpg01[mpg > median(mpg)]=1
mpg01
# unimos en un mismo conjunto de datos mpg01 y las otras variables de Auto:
Auto=data.frame(Auto,mpg01)
K = 5
filas=nrow(Auto)
particiones = round(nrow(Auto)/5)
id = rep(1:filas)
Auto2 = data.frame(id, Auto)

errors = rep(0, 5)
for(i in 0:(K-1)){
  train = Auto2$id[!(id>=(i*particiones)+1 & id<(particiones*(i+1))+1)]
  mpg01.test=mpg01[-train]
  Auto.train=Auto2[train, -ncol(Auto2)]
  Auto.test=Auto2[-train, -ncol(Auto2)]
  lda.fit=lda(mpg01~cylinders+weight+displacement+horsepower, data=Auto2[,-ncol(Auto2)], subset=train, family="binomial")
  predictor=predict(lda.fit, Auto.test)
  errors[i+1]=mean(predictor$class != mpg01.test)
}
mean(errors)


#para el apartado B) QDA:
errors = rep(0, 5)
for(i in 0:(K-1)){
  train = Auto2$id[!(id>=(i*particiones)+1 & id<(particiones*(i+1))+1)]
  mpg01.test=mpg01[-train]
  Auto.train=Auto2[train, -ncol(Auto2)]
  Auto.test=Auto2[-train, -ncol(Auto2)]
  qda.fit=qda(mpg01~cylinders+weight+displacement+horsepower, data=Auto2[,-ncol(Auto2)], subset=train, family="binomial")
  predictor=predict(qda.fit, Auto.test)
  errors[i+1]=mean(predictor$class != mpg01.test)
}
mean(errors)

#para el apartado C) RLG:
errors = rep(0, 5)
for(i in 0:(K-1)){
  train = Auto2$id[!(id>=(i*particiones)+1 & id<(particiones*(i+1))+1)]
  mpg01.test=mpg01[-train]
  Auto.train=Auto2[train, -ncol(Auto2)]
  Auto.test=Auto2[-train, -ncol(Auto2)]
  glm.fit=glm(mpg01~cylinders+weight+displacement+horsepower, data=Auto2[,-ncol(Auto2)], subset=train, family="binomial")
  probs=predict(glm.fit, Auto.test, type="response")
  predictor=rep(0, length(probs))
  predictor[probs > 0.5]=1
  errors[i+1]=mean(predictor!= mpg01.test)
}
mean(errors)

#para el apartado D) KNN:
library(boot)
library(class) 
attach(Auto)


mpg01=rep(0,nrow(Auto))
mpg01[mpg > median(mpg)]=1
mpg01
# unimos en un mismo conjunto de datos mpg01 y las otras variables de Auto:
Auto=data.frame(Auto,mpg01)

filas=nrow(Auto)
particiones = round(nrow(Auto)/5)
id = rep(1:filas)
Auto2 = data.frame(id, Auto)

errors = rep(0, 5)
set.seed(1)
for(i in 0:4){
  train = Auto2$id[!(id>=(i*particiones)+1 & id<(particiones*(i+1))+1)]
  mpg01.test=mpg01[-train]
  mpg01.train=mpg01[train]
  Auto.train=Auto2[train, -which(names(Auto2) %in% c("name"))]
  Auto.test=Auto2[-train, -which(names(Auto2) %in% c("name"))]
  knn.fit=knn(Auto.train, Auto.test, mpg01.train, k=100)
  errors[i+1]=mean(knn.fit!= mpg01.test)
}
mean(errors)

#Ejercicio3-> Usar la base de datos Boston para ajustar un modelo que prediga si dado un suburbio
#este tiene una tasa de criminalidad por encima o por debajo de la mediana.
#Comparar los modelos encontrados por RLG, LDA y QDA.
library(MASS)
install.packages("pls")
install.packages("leaps")
library(pls)
library(leaps)
attach(Boston)
#inicializamos semilla para la vc:
set.seed(1)
#definimos función para calcular error:
predict.regsubsets=function(object, newdata, id, ...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id = id)
  xvars=names(coefi)
  mat[, xvars] %*% coefi
}
#validación cruzada:
k = 10
folds=sample(1:k, nrow(Boston), replace = TRUE)
cv.errors=matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))

for (j in 1:k) {
  best.fit=regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred=predict(best.fit, Boston[folds == j, ], id = i)
    cv.errors[j, i]=mean((Boston$crim[folds == j] - pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error", pch=20, col='orange')

reg.best = regsubsets( crim ~. , data = Boston , nvmax =13)
#coef ( best.fit ,12)
coef ( reg.best ,12)

#Apartado 3.2-> Calcular  modelos RLG, LDA y QDA y valorar sus resultados:
attach(Boston)
crim01=rep(0,nrow(Boston))
#crim01
crim01[crim > median(crim)]=1
Boston=data.frame(Boston, crim01)
#pairs(Boston)

train=1:(nrow(Boston) / 2)
test =((nrow(Boston)/2) + 1):nrow(Boston)
Boston.train=Boston[train, ]
Boston.test=Boston[test, ]
crim01.test=crim01[test]

######## RLG #########
glm.fit=glm(crim01 ~ . - crim01 -crim , data = Boston, family = binomial, subset = train)

glm.probs=predict(glm.fit,Boston.test, type='response')
glm.pred=rep(0, length(glm.probs))
glm.pred[glm.probs>0.5]=1
#creamos matriz de confusión
table(glm.pred,crim01.test)
#predicciones correctas:
(68+139)/length(glm.probs)
#incorrectas:
1-((68+139)/length(glm.probs))

######## LDA #########
lda.fit =lda(crim01 ~ . - crim01 -crim , data = Boston, family = binomial, subset = train)
lda.pred = predict( lda.fit, Boston.test)
lda.class = lda.pred$class
table(lda.class , crim01.test)
#predicciones correctas:
(80+139)/253
#predicciones erróneas:
1- 0.8656126

######## QDA #########
qda.fit=qda(crim01 ~ . - crim01 -crim , data = Boston, family = binomial, subset = train)
#predecidmos usando modelo y datos test:
qda.class=predict(qda.fit ,Boston.test)$class
#creamos matriz:
table(qda.class,crim01.test)
#predicciones correctas:
(84+4)/253
#predicciones erróneas:
1-((84+4)/253)




