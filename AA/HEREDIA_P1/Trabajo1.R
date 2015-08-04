#antes de nada, debemos incluir la librería MASS, pues
#necesitaremos acceder a ella:
library(MASS)

#Ejercicio 1:
#Apartado 1.1-> Lee la descripción de la base de datos Boston
help(Boston)
#Apartado 1.2 ->
#en primer lugar cargamos los datos:
attach(Boston)
#y ahora hacemos los gráficos. 
#Gráfico 1: relación entre criminología y viviendas caras:
plot(tax, crim, pch=20, col= 'orange')
#Gráfico 2: relación entre criminología y zonas residenciales:
plot(zn, crim, pch=20, col= 'blue')
#Gráfico 3: relación entre prop. De óxido de nitrógeno en zona residencial
plot(zn, nox, pch=20, col= 'brown')

#Apartado 1.3 ->
ejercicio13=summary(lm(crim~. ,data=Boston))
ejercicio13

#Apartado 1.4 -> 
## a) alta tasa de criminalidad?
#rango que toma la variable
range(crim)
#suburbuios en los que el crimen supera la media
nrow(subset(Boston,crim>mean(crim)))
#suburbios en los que el crimen supera 10 veces la media
subset(Boston,crim>10*mean(crim))

## b) alta tasa de impuestos?
# rango que toma la variable
range(tax)
#suburburbios en los que los impuestos superan la media
nrow(subset(Boston,tax>mean(tax)))
#suburburbios en los que los impuestos superan 1.7 veces la media
nrow(subset(Boston,tax>1.7*mean(tax)))

## c) alta tasa de alumnos por profesor?
#rango que toma la variable
range(ptratio)
#suburbubios en los que nº alumnos/profesor supera la media
nrow(subset(Boston,ptratio>mean(ptratio)))
#suburbubios en los que nº alumnos/profesor supera 1.1 veces la media
subset(Boston,ptratio>1.1*mean(ptratio))

#Apartado 1.5 ->
#suburbios de Boston que bordea o cruza el rio Charles:
subset(Boston, chas==1)
#para que cuente el número de filas...
nrow(subset(Boston, chas==1))

#Apartado1.6 ->
#¿media de la tasa alumnos-profesor entre las ciudades de este conjunto?
mean(ptratio)

#Apartado1.7 ->
#suburbio que tiene el valor mediano más bajo de propietarios viviendo en sus casas:
subset(Boston,medv==min(medv))
#como se comparan estos valores con el rango global de los otros predictores:
#para obtener el valor de cada predictor en media:
colMeans(Boston)

#Apartado 1.8 ->
#suburbios tienen en promedio más de siete habitaciones por vivienda:
nrow(subset(Boston, rm > 7))
#suburbios tienen en promedio más de ocho habitaciones por vivienda:
nrow(subset(Boston, rm > 8))
#Haga algún comentario al caso cuyo promedio de habitaciones por vivienda sea mayor de ocho:
#para visualizar la información:
> subset(Boston, rm > 8)

#Ejercicio2:
#Apartado a) Predecir la ratio de crímenes per-capita usando las otras variables 
## i)Para cada predictor ajustar un modelo de regresión lineal simple con la variable respuesta
#predictor=Zona Residencial (zn):
rl_crim_zn=lm(crim~zn)
summary(rl_crim_zn)
#predictor=Zona Industrizal (indus):
rl_crim_indus=lm(crim~indus)
summary(rl_crim_indus)
#predictor=atravesado por río Charles (chas):
rl_crim_chas=lm(crim~chas)
summary(rl_crim_chas)
#predictor=concentración óxido nitroso (nox):
rl_crim_nox=lm(crim~nox)
summary(rl_crim_nox)
#predictor=media de habitaciones por vivienda (rm):
rl_crim_rm=lm(crim~rm)
summary(rl_crim_rm)
#predictor=antiguedad de la vivienda (age):
rl_crim_age=lm(crim~age)
summary(rl_crim_age)
#predictor=distancia a los centros (dis):
rl_crim_dis=lm(crim~dis)
summary(rl_crim_dis)
#predictor=accesibilidad a carreteras (rad):
rl_crim_rad=lm(crim~rad)
summary(rl_crim_rad)
#predictor=tasa de impuestos (tax):
rl_crim_tax=lm(crim~tax)
summary(rl_crim_tax)
#predictor=porcentaje alumnos/profesor (ptratio):
rl_crim_ptratio=lm(crim~ptratio)
summary(rl_crim_ptratio)
#predictor=proporción de personas negras (black):
rl_crim_black=lm(crim~black)
summary(rl_crim_black)
#predictor=índice pobreza (lstat):
rl_crim_lstat=lm(crim~lstat)
summary(rl_crim_lstat)
#predictor=valor mediano de viviendas ocupadas por sus propietarios (medv): 
rl_crim_medv=lm(crim~medv)
summary(rl_crim_medv)

## ii) ¿En qué modelos existe una asociación estadísticamente significativa entre predictor y respuesta?
#Criminalidad - índice de accesibilidad a carreteras: (crim - rad) ya que vimos en el apartado anterior
#que rad llega a explicar hasta el 39% de la criminalidad.
#Criminalidad - tasa de impuestos por vivienda: (crim - tax) ya que vimos en el apartado anterior que tax
#llega a explicar hasta el 33.83% de la criminalidad.
#Criminalidad - índice de pobreza: (crim - lstat) ya que vimos en el apartado anterior que tax puede llegar
#a explicar hasta el 20.6% de la criminalidad.

## iii) Crear algún gráfico que muestre los ajustes y que valide las respuestas anteriores.
#gráfica y recta para crim-chas:
plot(chas, crim, pch=20, col= 'green')
abline (rl_crim_chas ,lwd =3, col ="black")

#gráfica y recta para crim-zn:
plot(zn, crim, pch=20, col= 'violet')
abline (rl_crim_zn ,lwd =3, col ="black")

#gráfica y recta para crim-rad:
plot(rad, crim, pch=20, col= 'red')
abline (rl_crim_rad ,lwd =3, col ="black")

#gráfica y recta para crim-tax:
plot(tax, crim, pch=20, col= 'purple')
abline (rl_crim_tax ,lwd =3, col ="black")

#gráfica y recta para crim-lstat:
plot(lstat, crim, pch=20, col= 'cyan')
abline (rl_crim_lstat ,lwd =3, col ="black")

#Apartado b) Ajustar un modelo de regresión múltiple usando todos los predictores
summary(lm(crim~.,data=Boston ))
## i) Describir los resultados.
# (en el pdf)
## ii)¿Para qué predictores podemos rechazar la hipótesis nula, H 0 : Bj =0?
# (en el pdf)

#Apartado c) Comparación de los resultados encontrados en los dos puntos anteriores:
# x= vector con los coeficientes de cada una de las rl con un solo predictor:
x = c(rl_crim_zn$coefficients[2])
x = c(x,rl_crim_indus$coefficients[2])
x = c(x, rl_crim_chas$coefficients[2])
x = c(x, rl_crim_nox$coefficients[2])
x = c(x, rl_crim_rm$coefficients[2])
x = c(x, rl_crim_age$coefficients[2])
x = c(x, rl_crim_dis$coefficients[2])
x = c(x, rl_crim_rad$coefficients[2])
x = c(x, rl_crim_tax$coefficients[2])
x = c(x, rl_crim_ptratio$coefficients[2])
x = c(x, rl_crim_black$coefficients[2])
x = c(x, rl_crim_lstat$coefficients[2])
x = c(x, rl_crim_medv$coefficients[2])
# y=  vector con los coeficientes para los predictores de la repgresión múltiple:
y = c(Rm$coefficients[2])
for (i in 3:length(names(Boston)))
  y = c(y, Rm$coefficients[i])  
# dibujar gráfica:
plot(x,y, pch=20, col='brown3')


#Apartado d) 
#modelo lineal cúbico para variable predictor=zn
CuadraticoZN=(lm(crim~poly(zn,3),data=Boston))
#muestra información detallada de los modelos
summary(CuadraticoZN)
summary(rl_crim_zn)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_zn,CuadraticoZN) 

#modelo lineal cúbico para variable predictor=indus
CuadraticoINDUS=(lm(crim~poly(indus,3),data=Boston))
#muestra información detallada de los modelos
summary(CuadraticoINDUS)
summary(rl_crim_indus)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_indus,CuadraticoINDUS) 

#modelo lineal cúbico para variable predictor=chas
CuadraticoCHAS=(lm(crim~poly(chas,1),data=Boston))
#muestra información detallada de los modelos
summary(CuadraticoCHAS)
summary(rl_crim_chas)

#modelo lineal cúbico para variable predictor=nox
CuadraticoNOX=(lm(crim~poly(nox,3),data=Boston))
#muestra información detallada de los modelos
summary(CuadraticoNOX)
summary(rl_crim_nox)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_nox,CuadraticoNOX)

#modelo lineal cúbico para variable predictor=rm
CubicoRM=(lm(crim~poly(rm,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoRM)
summary(rl_crim_rm)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_rm,CubicoRM)

#modelo lineal cúbico para variable predictor=age
CubicoAGE=(lm(crim~poly(age,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoAGE)
summary(rl_crim_age)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_age,CubicoAGE)

#modelo lineal cúbico para variable predictor=dis
CubicoDIS=(lm(crim~poly(dis,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoDIS)
summary(rl_crim_dis)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_dis,CubicoDIS)

#modelo lineal cúbico para variable predictor=rad
CubicoRAD=(lm(crim~poly(rad,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoRAD)
summary(rl_crim_rad)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_rad,CubicoRAD)

#modelo lineal cúbico para variable predictor=tax
CubicoTAX=(lm(crim~poly(tax,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoTAX)
summary(rl_crim_tax)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_tax,CubicoTAX)

#modelo lineal cúbico para variable predictor=ptratio
CubicoP=(lm(crim~poly(ptratio,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoP)
summary(rl_crim_ptratio)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_ptratio,CubicoP)

#modelo lineal cúbico para variable predictor=black
CubicoBlack=(lm(crim~poly(black,3),data=Boston))
#muestra información detallada de los modelos
summary(CubicoBlack)
summary(rl_crim_black)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_black,CubicoBlack)

#modelo lineal cúbico para variable predictor=lstat
Cubicolstat=(lm(crim~poly(lstat,3),data=Boston))
#muestra información detallada de los modelos
summary(Cubicolstat)
summary(rl_crim_lstat)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_lstat,Cubicolstat)

#modelo lineal cúbico para variable predictor=medv
Cubicomedv=(lm(crim~poly(medv,3),data=Boston))
#muestra información detallada de los modelos
summary(Cubicomedv)
summary(rl_crim_medv)
#test anova para comparar ajuste cuadrático y lineal
anova(rl_crim_medv,Cubicomedv)

#Ejercicio3: 
library(ISLR)
attach (Auto)
help(Auto)
#Apartado 3.1 -> representación gráfica matricial (“scatterplot”) que incluya todas las
#representaciones de cada dos variables del conjunto de datos.
pairs(Auto)
#Apartado 3.2 -> Calcular la matriz de correlaciones entre variables cuantitativas:
#como names no es una variable cuantitativa, no la tenemos en cuenta:
cor(Auto[, -9]) 
#Apartado 3 ->
#regresión lineal múltiple usando mpg como la respuesta y  la demás variables como  predictores (excepto “name”)
summary(lm(mpg~.-name,data=Auto))
#¿Que sugiere el coeficiente para la variable “year”?
summary(lm(mpg~year,data=Auto))

#Apartado 4 -> obtener los intervalos de confianza al 95% para los coeficientes:
confint(lm(mpg~.-name,data=Auto),level=0.95)

#Apartado 5 -> realizar dibujos de diagnóstico sobre la regresión lineal:
#le pongo nombre a la RLM anterior:
RLMcoches=lm(mpg~.-name,data=Auto)
#pinto residuos rstudentizados frente a valores ajustados: 
plot ( RLMcoches, pch=20, col="darkgoldenrod2" )

#Apartado 6 -> Usar los símbolos “*” y “:” de R para ajustar un modelo de regresión lineal con
#términos de interacción:
summary(lm(mpg~year*origin:year*acceleration+horsepower*weight))


