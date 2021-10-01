######################################
##      I1 PRACTICA EYP2905         ##
## Cristian Dibán - Bárbara Ramírez ##
######################################
library(rio)
ventas <- rio::import("Venta.xlsx")
#names(ventas)
Xt <- ts(ventas$`Venta Credito (MM $CL)`,start = 2018, frequency = 12)


## Diagnostico y Modelamiento ----
acf(Xt, lag.max = 40, ylim=c(-1,1)) #hay dependencia serial
LSTS::periodogram(Xt) #no se ven estacionalidades


Data <- data.frame(v_credito_MM = c(Xt), tiempo = c(time(Xt)), 
                   meses = ventas$MONTH)

plot(v_credito_MM ~ tiempo, data=Data, type="p",pch=16,
     ylim=c(0,120000),xlim=c(2018,2022), xlab="Tiempo",
     ylab="", main="Ventas de Crédito en MM$",las=1, bty="n",
     col="blue")
lines(v_credito_MM ~ tiempo, data=Data,lwd=2)
abline(v=2019.5,lty=2,col="blue");abline(v=2020.583,lty=2,col="blue") 
#cambio de pendiente en Julio/2019 y Agosto/2020


## Introducimos variables Dummy

Data$dummy = c(rep(0,18),rep(1,13),rep(2,9))

#Dado que existe un cambio de pendiente, utilizaremos
#dummy multiplicativa (definiendo la variable dummy como factor)

fit1 <- lm(v_credito_MM ~ tiempo*as.factor(dummy), data=Data)
Data$fitted1 <- as.vector(fit1$fitted.values)
plot(v_credito_MM ~ tiempo, data=Data, type="l", pch=20,
     ylim=c(0,120000),xlim=c(2018,2022), xlab="Tiempo",
     ylab="", main="Ventas de Crédito en MM$",las=1, bty="n",lwd=2)
lines(fitted1 ~ tiempo, data=Data, col="red", lwd=2)
legend("bottomright", legend=c("Valor real","Valor ajustado"),
       col=c("black","red"),lwd=2, bty="n",cex=0.8)
acf(fit1$res,lag.max=30) 
#No se logra modelar de buena forma la tendencia
#y sigue habiendo deoendencia serial.


## Introducimos un nuevo predictor: IMACEC + factor "mes"
imacec <- rio::import("IMACEC.xlsx")
Data$imacec =imacec$`1.Imacec`[1:40]

#Nuevo modelo
fit2=(lm(v_credito_MM ~ tiempo*dummy + 
            as.factor(meses) + imacec, data=Data))
Data$fitted2 <- as.vector(fit2$fitted.values)
plot(v_credito_MM ~ tiempo, data=Data, type="l", pch=20,
     ylim=c(0,120000),xlim=c(2018,2022), xlab="Tiempo",
     ylab="", main="Ventas de Crédito en MM$",las=1, bty="n",lwd=2)
lines(fitted2 ~ tiempo, data=Data, col="orange",lwd=2)
legend("bottomright", legend=c("Valor real","Valor ajustado"),
       col=c("black","orange"),lwd=2, bty="n", cex=0.8)
#buen ajuste
acf(fit2$res,lag.max=40,ylim=c(-1,1)) #se elimina la dep. serial
plot(fit2$res,type="l",ylim=c(-20000,20000));abline(h=0)
#se observa varianza constante y media 0
mean(fit2$res)


## Predicción ----
aux1 <- data.frame(newtime = seq(2021+3/12,2022,1/12))
aux2 <- as.vector(predict(fit2, tiempo = aux1 ))

plot(v_credito_MM ~ tiempo, data=Data, type="l", pch=20,
     ylim=c(0,120000),xlim=c(2018,2022), xlab="Tiempo",
     ylab="", main="Ventas de Crédito en MM$",las=1, bty="n",lwd=2)
lines(fitted2 ~ tiempo, data=Data, col="orange",lwd=2)
lines(aux2[1:dim(aux1)[1]]~aux1$newtime, col="darkred", lwd=2)
legend("bottomright", legend=c("Valor real","Valor ajustado","Predicción"),
       col=c("black","orange","darkred"),lwd=2, bty="n",cex=0.8)

## Medidas de calidad del ajuste (comparando fit 1 y fit2) ----

#MAPE
MAPE <- function(original, ajuste){
   aux <- mean(abs((original-ajuste)/original))
   return(aux)
}
MAPE(Xt,Data$fitted1);MAPE(Xt,Data$fitted2)
#0.1219029 ; 0.08222443 -> fit2 es mejor

#AIC
AIC(fit1);AIC(fit2)
#857.2727 ; 834.5213


#R-squared
summary(fit1) #R^2 = 0.5777 
summary(fit2) #R^2 = 0.7945 



## Diagnostico de Residuos ----

#Normalidad:
shapiro.test(fit2$res) #p-value = 0.9552
ks.test(scale(fit2$res), "pnorm") #p-value = 0.9727
qqnorm(fit2$res)
qqline(fit2$res, lwd = 2, col = "red")
#no se rechaza H0: hay Normalidad

#Homocedasticidad:
library(lmtest)
bptest(fit2$res~ Data$tiempo) #p-value = 0.07189
#no se rechaza H0: residuos Homocedasticos

#Incorrelación:
library(LSTS)
ts.diag(fit2$res) 
#se rechaza H0: residuos incorrelacionados
#-> residuos correlacionados



#===========
library(splines)

#regresion + regresion spline
fit <- lm(v_credito_MM ~ tiempo*dummy +imacec + 
             bs(meses, degree =11), data = Data) 
Data$fitted <- fit$fitted.values
plot(v_credito_MM ~ tiempo, data=Data, type="l", pch=20,
     ylim=c(0,120000),xlim=c(2018,2022))
lines(fitted ~ tiempo, data = Data, col = "red",lwd=2)
MAPE(Data$v_credito_MM,Data$fitted)

#regresion con dummy
fit2=(lm(v_credito_MM ~ tiempo*dummy + 
            as.factor(meses) + imacec, data=Data))
Data$fitted2 <- as.vector(fit2$fitted.values)
plot(v_credito_MM ~ tiempo, data=Data, type="l", pch=20,
     ylim=c(0,120000),xlim=c(2018,2022))
lines(fitted2 ~ tiempo, data=Data, col="orange",lwd=2)