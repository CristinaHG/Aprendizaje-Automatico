###############################################
#TRABAJO 3   Aprendizaje Automático           #
# Mª Cristina Heredia Gómez                   #
#3º de Ing.Informática - Univerdad de Granada #
###############################################

####EJERCICIO1####
library(ISLR)
attach(OJ)
help(OJ)
#Apartado1:
set.seed(1)
nrow(OJ)
train0=sample(nrow(OJ),800)
train=OJ[train0,]
test=OJ[-train0,]

dim(test)
dim(train)
etiqueta=train[,1]
train=train[ , -1]

dat=data.frame(train, etiqueta=as.factor(etiqueta))
library(e1071)
svmfit = svm( etiqueta~. ,data =dat, kernel ="linear" , cost =0.01 )

#svmfit = svm( Purchase~. ,data =train, kernel ="linear" , cost =0.01 )

summary(svmfit)
#Apartado2->
#error training:
predict.train = predict(svmfit,train)
table(predict.train,etiqueta)
#hacemos cálculo del error:
(55+78)/(439+228+55+78)

#error test:
predict.test = predict(svmfit,test)
table(predict.test,test$Purchase)
#hacemos cálculo del error:
(18+31)/(141+31+80+18)

set.seed(1)
train0=sample(nrow(OJ),800)
train=OJ[train0,]
test=OJ[-train0,]
#Apartado3->
set.seed(1)
tune.out=tune(svm,Purchase~. , data =train, kernel ="linear" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)))
bestmod=tune.out$best.model
summary(bestmod)
#pintamos curvas ROCR:
#recargamos arriba la parte de los subconjuntos que nos interesa:
set.seed(1)
train0=sample(nrow(OJ),800)
train=OJ[train0,]
test=OJ[-train0,]
library(e1071)
library(ROCR)
#creamos función rocplot:
rocplot = function( pred , truth , ...) {
predob = prediction ( pred , truth )
perf = performance( predob , "tpr" , "fpr")
plot ( perf, ...) }

dev.off()
par(mfrow=c(1,1))

#COSTE=0.001
svmfit.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =0.001 , decision.values=T )
fitted = attributes( predict(svmfit.opt, train, decision.value=TRUE))$decision.values
rocplot(fitted ,OJ[train0,"Purchase"], main=" Training Data" ,col="red",add=par("new"))
par(new=T)
#COSTE=0.01
svmfit2.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =0.01 , decision.values=T )
fitted2 = attributes( predict(svmfit2.opt, train, decision.value=TRUE))$decision.values
rocplot(fitted2 ,OJ[train0,"Purchase"], main=" Training Data" ,col="navy",add=par("new"))
par(new=T)
#COSTE=0.1
svmfit3.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =0.1 , decision.values=T )
fitted3 = attributes( predict(svmfit3.opt, train, decision.value=TRUE))$decision.values
rocplot(fitted3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="green",add=par("new"))
par(new=T)
#COSTE=1
svmfit4.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =1 , decision.values=T )
fitted4 = attributes( predict(svmfit4.opt, train, decision.value=TRUE))$decision.values
rocplot(fitted4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="orange",add=par("new"))
par(new=T)
#COSTE=10
svmfit5.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =10 , decision.values=T )
fitted5 = attributes( predict(svmfit5.opt, train, decision.value=TRUE))$decision.values
rocplot(fitted5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="pink",add=par("new"))
par(new=T)

legend(0.70, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("red","blue","green","orange","pink"),
       text.col=c("red","navy","green","orange","pink"), lty=1,
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")


#apartado 4->
svmfit2.opt = svm( Purchase~. , data = train , kernel ="linear" , cost =0.01  )
#error training:
predict.train2 = predict(svmfit2.opt,train)
table(predict.train2, train$Purchase)
#hacemos cálculo del error:
(55+78)/(439+228+55+78)

#error test:
predict.test2 = predict(svmfit2.opt,test)
table(predict.test2,test$Purchase)
#hacemos cálculo del error:
(18+31)/(141+80+18+31)

#apartado5-> 
#repetición apartado 2 con gamma en el rango [10, 1, 0.1, 0.01, 0.001]

#GAMMA=10:
##COSTE=0.001
svmfit1.rad = svm( Purchase~. , data = train , kernel ="radial" ,gamma=10, cost =0.001 , decision.values=T )
fittedrad1 = attributes( predict(svmfit1.rad, train, decision.value=TRUE))$decision.values
rocplot(fittedrad1 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)
summary(svmfit1.rad)
#error training:
predict.trainRad1 = predict(svmfit1.rad,train)
table(predict.trainRad1, train$Purchase)
#hacemos cálculo del error:
(306)/(494+306)

#error test:
predict.testRad1 = predict(svmfit1.rad,test)
table(predict.testRad1,test$Purchase)
#hacemos cálculo del error:
(111)/(159+111)

#coste óptimo para gamma=10:
set.seed(2)
tune.outRad10=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)), gamma=10)
bestmodRad10=tune.outRad10$best.model
summary(bestmodRad10)
#ajustados modelos con ese coste optimo para gamma=10:
svmfitRad.10best = svm( Purchase~. , data = train , kernel ="radial" ,gamma=10, cost =1 , decision.values=T )
fittedrad10 = attributes( predict(svmfitRad.10best, train, decision.value=TRUE))$decision.values
rocplot(fittedrad10 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)

#error training:
predict.trainRad10 = predict(svmfitRad.10best,train)
table(predict.trainRad10, train$Purchase)
#hacemos cálculo del error:
(12+37)/(482+269+12+37)

#error test:
predict.testRad10 = predict(svmfitRad.10best,test)
table(predict.testRad10,test$Purchase)
#hacemos cálculo del error:
(17+47)/(142+47+17+64)

#GAMMA=1:
#coste óptimo para gamma=1:
set.seed(2)
tune.outRad1=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),gamma=1)
bestmodRad1=tune.outRad1$best.model
summary(bestmodRad1)
#ajustados modelos con ese coste optimo para gamma=1:
svmfitRad.1best = svm( Purchase~. , data = train , kernel ="radial" ,gamma=1, cost =1 , decision.values=T )
fittedrad1 = attributes( predict(svmfitRad.1best, train, decision.value=TRUE))$decision.values
rocplot(fittedrad1 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)

#error training:
predict.trainRad1 = predict(svmfitRad.1best,train)
table(predict.trainRad1, train$Purchase)
#hacemos cálculo del error:
(35+51)/(459+255+35+51)

#error test:
predict.testRad1 = predict(svmfitRad.1best,test)
table(predict.testRad1,test$Purchase)
#hacemos cálculo del error:
(23+33)/(136+33+23+78)


#GAMMA=0.1:
#coste óptimo para gamma=1:
set.seed(2)
tune.outRad0.1=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),gamma=0.1)
bestmodRad0.1=tune.outRad0.1$best.model
summary(bestmodRad0.1)

#ajustados modelos con ese coste optimo para gamma=0.1:
svmfitRad0.1best = svm( Purchase~. , data = train , kernel ="radial" ,gamma=0.1, cost =1 , decision.values=T )
fittedrad0.1 = attributes( predict(svmfitRad0.1best, train, decision.value=TRUE))$decision.values
rocplot(fittedrad0.1 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)

#error training:
predict.trainRad0.1 = predict(svmfitRad0.1best,train)
table(predict.trainRad0.1, train$Purchase)
#hacemos cálculo del error:
(39+75)/(455+231+39+75)

#error test:
predict.testRad0.1 = predict(svmfitRad0.1best,test)
table(predict.testRad0.1,test$Purchase)
#hacemos cálculo del error:
(17+28)/(142+83+17+28)


#GAMMA=0.01:
#coste óptimo para gamma=0.01:
set.seed(2)
tune.outRad0.01=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),gamma=0.01)
bestmodRad0.01=tune.outRad0.01$best.model
summary(bestmodRad0.01)

#ajustados modelos con ese coste optimo para gamma=0.01:
svmfitRad0.01best = svm( Purchase~. , data = train , kernel ="radial" ,gamma=0.01, cost =1 , decision.values=T )
fittedrad0.01 = attributes( predict(svmfitRad0.01best, train, decision.value=TRUE))$decision.values
rocplot(fittedrad0.01 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)

#error training:
predict.trainRad0.01 = predict(svmfitRad0.01best,train)
table(predict.trainRad0.01, train$Purchase)
#hacemos cálculo del error:
(51+75)/(443+231+51+75)

#error test:
predict.testRad0.01 = predict(svmfitRad0.01best,test)
table(predict.testRad0.01,test$Purchase)
#hacemos cálculo del error:
(15+32)/(144+79+15+32)


#GAMMA=0.001:
#coste óptimo para gamma=0.001:
set.seed(2)
tune.outRad0.001=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),gamma=0.001)
bestmodRad0.001=tune.outRad0.001$best.model
summary(bestmodRad0.001)

#ajustados modelos con ese coste optimo para gamma=0.001:
svmfitRad0.001best = svm( Purchase~. , data = train , kernel ="radial" ,gamma=0.001, cost =10 , decision.values=T )
fittedrad0.001 = attributes( predict(svmfitRad0.001best, train, decision.value=TRUE))$decision.values
rocplot(fittedrad0.001 ,OJ[train0,"Purchase"], main=" Training Data" ,colorize=T)

#error training:
predict.trainRad0.001 = predict(svmfitRad0.001best,train)
table(predict.trainRad0.001, train$Purchase)
#hacemos cálculo del error:
(58+72)/(436+234+58+72)

#error test:
predict.testRad0.001 = predict(svmfitRad0.001best,test)
table(predict.testRad0.001,test$Purchase)
#hacemos cálculo del error:
(19+30)/(140+81+19+30)

#mejor de todos los gamma y todos los costes:
set.seed(1)
tune.outRad=tune(svm, Purchase~. , data =train, kernel ="radial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10),gamma=c(10, 1, 0.1, 0.01, 0.001)))
bestmodRad=tune.outRad$best.model
summary(bestmodRad)
summary(tune.outRad)

#Apartado6-> núcleo polinómico
dev.off()
par(mfrow=c(1,1))

# ->con DEGREE=2<-
#coste=0.001:
set.seed(2)
svmfitPOL0.001 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.001 ,degree=2, decision.values=T )
fittedPOL0.001 = attributes( predict(svmfitPOL0.001, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.001 ,OJ[train0,"Purchase"], main=" Training Data" ,col="blue2",add=par("new"))
par(new=T)

#coste=0.01:
set.seed(2)
svmfitPOL0.01 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.01 ,degree=2, decision.values=T )
fittedPOL0.01 = attributes( predict(svmfitPOL0.01, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.01 ,OJ[train0,"Purchase"], main=" Training Data" ,col="forestgreen",add=par("new"))
par(new=T)

#coste=0.1:
set.seed(2)
svmfitPOL0.1 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.1 ,degree=2, decision.values=T )
fittedPOL0.1 = attributes( predict(svmfitPOL0.1, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.1 ,OJ[train0,"Purchase"], main=" Training Data" ,col="darkorange1",add=par("new"))
par(new=T)

#coste=1:
set.seed(2)
svmfitPOL1 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=2, decision.values=T,scale=T )
fittedPOL1 = attributes( predict(svmfitPOL1, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL1 ,OJ[train0,"Purchase"], main=" Training Data" ,col="darkorchid1",add=par("new"))
par(new=T)

#coste=10:
set.seed(2)
svmfitPOL10 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=2, decision.values=T,scale=T )
fittedPOL10 = attributes( predict(svmfitPOL10, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL10 ,OJ[train0,"Purchase"], main=" Training Data" ,col="firebrick2",add=par("new"))
par(new=T)

legend(0.66, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("blue2","forestgreen","darkorange1","darkorchid1","firebrick2"),
       text.col=c("blue2","forestgreen","darkorange1","darkorchid1","firebrick2"), lty=1, 
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")

dev.off()

#mejor cost para degree=2:
set.seed(2)
tune.outPol2=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),degree=2)
bestmodPol2=tune.outPol2$best.model
summary(bestmodPol2)

#ajustamos el modelo anterior que tune nos ha dicho ( degree=2, cost=10)
svmfitPol2best = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=2, decision.values=T )
#error training:
predict.trainPol2best = predict(svmfitPol2best,train)
table(predict.trainPol2best, train$Purchase)
#hacemos cálculo del error:
(44+72)/(450+234+44+72)

#error test:
predict.testPol2best = predict(svmfitPol2best,test)
table(predict.testPol2best,test$Purchase)
#hacemos cálculo del error:
(19+31)/(140+80+19+31)

dev.off()
#->con DEGREE=3<-
#coste=0.001:
set.seed(2)
svmfitPOL0.001D3 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.001 ,degree=3, decision.values=T ,scale=T)
fittedPOL0.001D3 = attributes( predict(svmfitPOL0.001D3, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.001D3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="green1",add=par("new"))
par(new=T)

#coste=0.01:
set.seed(2)
svmfitPOL0.01D3 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.01 ,degree=3, decision.values=T,scale=T )
fittedPOL0.01D3 = attributes( predict(svmfitPOL0.01D3, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.01D3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="mediumpurple",add=par("new"))
par(new=T)

#coste=0.1:
set.seed(2)
svmfitPOL0.1D3 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.1 ,degree=3, decision.values=T,scale=T )
fittedPOL0.1D3 = attributes( predict(svmfitPOL0.1D3, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.1D3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="red3",add=par("new"))
par(new=T)

#coste=1:
set.seed(2)
svmfitPOL1D3 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=3, decision.values=T, scale=T )
fittedPOL1D3 = attributes( predict(svmfitPOL1D3, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL1D3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="yellow1",add=par("new"))
par(new=T)

#coste=10:
set.seed(2)
svmfitPOL10D3 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=3, decision.values=T,scale=T )
fittedPOL10D3 = attributes( predict(svmfitPOL10D3, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL10D3 ,OJ[train0,"Purchase"], main=" Training Data" ,col="yellowgreen",add=par("new"))
par(new=T)
summary(svmfitPOL10D3)
legend(0.66, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("green1","mediumpurple","red3","yellow1","yellowgreen"),
       text.col=c("green1","mediumpurple","red3","yellow1","yellowgreen"), lty=1, 
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")


#mejor cost para degree=3:
set.seed(2)
tune.outPol3=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),degree=3)
bestmodPol3=tune.outPol3$best.model
summary(bestmodPol3)

#ajustamos el modelo anterior que tune nos ha dicho ( degree=3, cost=1)
svmfitPol3best = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=3, decision.values=T )
#error training:
predict.trainPol3best = predict(svmfitPol3best,train)
table(predict.trainPol3best, train$Purchase)
#hacemos cálculo del error:
(30+93)/(464+213+30+93)

#error test:
predict.testPol3best = predict(svmfitPol3best,test)
table(predict.testPol3best,test$Purchase)
#hacemos cálculo del error:
(41+13)/(146+41+13+70)


dev.off()
#->con DEGREE=4<-
#coste=0.001:
set.seed(2)
svmfitPOL0.001D4 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.001 ,degree=4, decision.values=T )
fittedPOL0.001D4 = attributes( predict(svmfitPOL0.001D4, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.001D4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="chartreuse3",add=par("new"))
par(new=T)

#coste=0.01:
set.seed(2)
svmfitPOL0.01D4 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.01 ,degree=4, decision.values=T)
fittedPOL0.01D4 = attributes( predict(svmfitPOL0.01D4, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.01D4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="chocolate2",add=par("new"))
par(new=T)

#coste=0.1:
set.seed(2)
svmfitPOL0.1D4 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.1 ,degree=4, decision.values=T,scale=T )
fittedPOL0.1D4 = attributes( predict(svmfitPOL0.1D4, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.1D4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="firebrick2",add=par("new"))
par(new=T)

#coste=1:
set.seed(2)
svmfitPOL1D4 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=4, decision.values=T )
fittedPOL1D4 = attributes( predict(svmfitPOL1D4, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL1D4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="goldenrod1",add=par("new"))
par(new=T)

#coste=10:
set.seed(2)
svmfitPOL10D4 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=4, decision.values=T )
fittedPOL10D4 = attributes( predict(svmfitPOL10D4, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL10D4 ,OJ[train0,"Purchase"], main=" Training Data" ,col="dodgerblue3",add=par("new"))
par(new=T)

legend(0.72, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("chartreuse3","chocolate2","firebrick2","goldenrod1","dodgerblue3"),
       text.col=c("chartreuse3","chocolate2","firebrick2","goldenrod1","dodgerblue3"), lty=1, 
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")

#mejor cost para degree=4:
set.seed(2)
tune.outPol4=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),degree=4)
bestmodPol4=tune.outPol4$best.model
summary(bestmodPol4)

#ajustamos el modelo anterior que tune nos ha dicho ( degree=4, cost=10)
svmfitPol4best = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=4, decision.values=T )
#error training:
predict.trainPol4best = predict(svmfitPol4best,train)
table(predict.trainPol4best, train$Purchase)
#hacemos cálculo del error:
(32+80)/(462+226+80+32)

#error test:
predict.testPol4best = predict(svmfitPol4best,test)
table(predict.testPol4best,test$Purchase)
#hacemos cálculo del error:
(15+37)/(144+74+15+37)

dev.off()
# ->con DEGREE=5<-
#coste=0.001:
set.seed(2)
svmfitPOL0.001D5 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.001 ,degree=5, decision.values=T )
fittedPOL0.001D5 = attributes( predict(svmfitPOL0.001D5, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.001D5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="blue2",add=par("new"))
par(new=T)

#coste=0.01:
set.seed(2)
svmfitPOL0.01D5 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.01 ,degree=5, decision.values=T )
fittedPOL0.01D5 = attributes( predict(svmfitPOL0.01D5, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.01D5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="forestgreen",add=par("new"))
par(new=T)

#coste=0.1:
set.seed(2)
svmfitPOL0.1D5 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.1 ,degree=5, decision.values=T )
fittedPOL0.1D5 = attributes( predict(svmfitPOL0.1D5, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.1D5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="darkorange1",add=par("new"))
par(new=T)

#coste=1:
set.seed(2)
svmfitPOL1D5 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=5, decision.values=T)
fittedPOL1D5 = attributes( predict(svmfitPOL1D5, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL1D5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="darkorchid1",add=par("new"))
par(new=T)

#coste=10:
set.seed(2)
svmfitPOL10D5 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=5, decision.values=T,scale=T)
fittedPOL10D5 = attributes( predict(svmfitPOL10D5, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL10D5 ,OJ[train0,"Purchase"], main=" Training Data" ,col="firebrick2",add=par("new"))
par(new=T)

legend(0.66, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("blue2","forestgreen","darkorange1","darkorchid1","firebrick2"),
       text.col=c("blue2","forestgreen","darkorange1","darkorchid1","firebrick2"), lty=1, 
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")

#mejor cost para degree=5:
set.seed(1)
tune.outPol5=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),degree=5)
bestmodPol5=tune.outPol5$best.model
summary(bestmodPol5)

#ajustamos el modelo anterior que tune nos ha dicho ( degree=5, cost=10)
svmfitPol5best = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=5, decision.values=T )
#error training:
predict.trainPol5best = predict(svmfitPol5best,train)
table(predict.trainPol5best, train$Purchase)
#hacemos cálculo del error:
(28+91)/(466+215+28+91)

#error test:
predict.testPol5best = predict(svmfitPol5best,test)
table(predict.testPol5best,test$Purchase)
#hacemos cálculo del error:
(17+43)/(142+68+17+43)

dev.off()
#->con DEGREE=6<-
#coste=0.001:
set.seed(2)
svmfitPOL0.001D6 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.001 ,degree=6, decision.values=T )
fittedPOL0.001D6 = attributes( predict(svmfitPOL0.001D6, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.001D6 ,OJ[train0,"Purchase"], main=" Training Data" ,col="chartreuse3",add=par("new"))
par(new=T)

#coste=0.01:
set.seed(2)
svmfitPOL0.01D6 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.01 ,degree=6, decision.values=T)
fittedPOL0.01D6 = attributes( predict(svmfitPOL0.01D6, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.01D6 ,OJ[train0,"Purchase"], main=" Training Data" ,col="chocolate2",add=par("new"))
par(new=T)

#coste=0.1:
set.seed(2)
svmfitPOL0.1D6 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =0.1 ,degree=6, decision.values=T)
fittedPOL0.1D6 = attributes( predict(svmfitPOL0.1D6, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL0.1D6 ,OJ[train0,"Purchase"], main=" Training Data" ,col="firebrick2",add=par("new"))
par(new=T)

#coste=1:
set.seed(2)
svmfitPOL1D6 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =1 ,degree=6, decision.values=T )
fittedPOL1D6 = attributes( predict(svmfitPOL1D6, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL1D6 ,OJ[train0,"Purchase"], main=" Training Data" ,col="goldenrod1",add=par("new"))
par(new=T)

#coste=10:
set.seed(2)
svmfitPOL10D6 = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=6, decision.values=T )
fittedPOL10D6 = attributes( predict(svmfitPOL10D6, train, decision.value=TRUE))$decision.values
rocplot(fittedPOL10D6 ,OJ[train0,"Purchase"], main=" Training Data" ,col="dodgerblue3",add=par("new"))
par(new=T)

legend(0.70, 0.48,paste("coste=",c(0.001,0.01,0.1,1,10)), col=c("chartreuse3","chocolate2","firebrick2","goldenrod1","dodgerblue3"),
       text.col=c("chartreuse3","chocolate2","firebrick2","goldenrod1","dodgerblue3"), lty=1, 
       merge=TRUE, bg="honeydew", cex=0.6, title="Costes:" ,title.col="black")

#mejor cost para degree=6:
set.seed(2)
tune.outPol6=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10)),degree=6)
bestmodPol6=tune.outPol6$best.model
summary(bestmodPol6)

#ajustamos el modelo anterior que tune nos ha dicho ( degree=6, cost=10)
svmfitPol6best = svm( Purchase~. , data = train , kernel ="polynomial" , cost =10 ,degree=6, decision.values=T )
#error training:
predict.trainPol6best = predict(svmfitPol6best,train)
table(predict.trainPol6best, train$Purchase)
#hacemos cálculo del error:
(25+101)/(469+205+25+101)

#error test:
predict.testPol6best = predict(svmfitPol6best,test)
table(predict.testPol6best,test$Purchase)
#hacemos cálculo del error:
(18+46)/(141+65+18+46)

#mejor cost para cualquier degree:(mejor ajuste para cualquier ajuste de SVM con núcleo polinomial)
set.seed(2)
tune.outP=tune(svm, Purchase~. , data =train, kernel ="polynomial" ,ranges=list(cost=c(0.001,0.01,0.1,1,10),degree=c(2,3,4,5,6)))
bestmodPol=tune.outP$best.model
summary(bestmodPol)



####EJERCICIO2####

#apartado1->
library(ISLR)
library(tree)
attach(OJ)
set.seed(1)
train0=sample(nrow(OJ),800)
train=OJ[train0,]
test=OJ[-train0,]

tree.OJ=tree(Purchase~. , OJ, subset=train0)

#apartado2->
summary(tree.OJ)
tree.OJ

#apartado3->
plot(tree.OJ)
text(tree.OJ,pretty=0)

#apartado4->
tree.pred=predict(tree.OJ, test , type ="class" )
table(tree.pred,test$Purchase)
#calculamos tasa de error:
(12+49)/(147+49+12+62)
#calculamos precisión:
1-0.2259259

#apartado5->

#óptimo diciéndole que tome el error de clasificación
set.seed(1)
cv.OJ=cv.tree(tree.OJ,FUN=prune.misclass)
cv.OJ

#pintamos error frente a size y error frente a k:
par ( mfrow = c (1 ,2) )
plot ( cv.OJ$size , cv.OJ$dev,type ="b", pch=20,col="darkgreen")
plot ( cv.OJ$k , cv.OJ$dev,type ="b",pch=20,col="pink")

#apartado6->
dev.off()
plot(cv.OJ$size, cv.OJ$dev, type ="b", pch=20, col="cornflowerblue", xlab="tamaño", ylab="error CV")

#apartado7->
#podamos y pintamos el árbol:
prune.OJ= prune.misclass(tree.OJ, best =5)
plot( prune.OJ)
text( prune.OJ , pretty =0)

#error training:
summary(prune.OJ)
#error de test:
tree.predPODA=predict(prune.OJ, test , type ="class" )
table(tree.predPODA,test$Purchase)
#calculamos tasa de error:
(12+49)/(147+62+12+49)
#calculamos precisión:
1-0.2259259


####EJERCICIO3####
#apartado1->
library(ISLR)
attach(Hitters)
names(Hitters)
#¿cuántas observaciones faltan?
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
sum(is.na(Hitters))

#aplicamos transformación logaritmica al resto de valores del salario:
Hitters$Salary=log(Hitters$Salary)

#creamos conjunto training y test:
set.seed(1)
trainHit=sample(nrow(Hitters),200)
train=Hitters[trainHit,]
test=Hitters[-trainHit,]

#apartado2->
library(gbm)
set.seed(1)
lambda=10^seq(-3, -0.2, by = 0.01)
lambda.tam=length(lambda)
mse=rep(NA,lambda.tam)
for (i in (1:lambda.tam)) {
  boosting.HITTERS= gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred.trainHITTERS=predict(boosting.HITTERS, train, n.trees = 1000)
  mse[i]=mean((pred.trainHITTERS - train$Salary)^2)
}
plot(lambda, mse, type = "b", xlab = "lambda", ylab = " MSE para training", pch=20, col="darkred")

#apartado3->
####BOOSTING:
set.seed(1)
lambda=10^seq(-3, -0.2, by = 0.01)
lambda.tam=length(lambda)
mse.test=rep(NA,lambda.tam)
for (i in (1:lambda.tam)) {
  boosting.HITTERS= gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred.testHITTERS=predict(boosting.HITTERS, test, n.trees = 1000)
  mse.test[i]=mean((pred.testHITTERS - test$Salary)^2)
}
plot(lambda, mse.test, type = "b", xlab = "lambda", ylab = " MSE para test", pch=20, col="darkmagenta")

#quién da el mse más chico:
which.min(mse.test)
lambda[85]

min(mse.test)
####REGRESIÓN MÚLTIPLE:
#ajustamos modelos RM para datos train:
lm.fit=lm(Salary~. , data=train)
#predecimos datos test:
predict.lm=predict(lm.fit, test)
#table(predict.lm, test$Salary)
mean((predict.lm - test$Salary)^2)

#LASSO:
library(glmnet)
lambda=10^seq(-3, -0.2, by = 0.01)
x=model.matrix(Salary~., train)
x.test=model.matrix(Salary~., test)

lasso.mod=glmnet(x,train$Salary,alpha=1,lambda=lambda)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x,train$Salary,alpha=1)
#plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam, newx=x.test )
mean((lasso.pred -test$Salary))
bestlam

#apartado4-> 
#ajustamos modelo boosting para mejor lambda:
boosting.best= gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage =lambda[which.min(mse.test)])
summary(boosting.best)

#apartado5->
library(randomForest)
set.seed(1)
bag.HITTERS = randomForest( Salary~. , data =train, mtry =19 , importance = TRUE )
yhat.bag = predict( bag.HITTERS , newdata = test)
plot(yhat.bag,test$Salary, pch=20, col="olivedrab2")
abline(0,1,col="black")
mean((yhat.bag - test$Salary)^2)
