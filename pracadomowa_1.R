dane <- read.csv("PKNORLEN.mst", sep=",",header=TRUE)
dane <- dane[which(dane[,2]>=20190101),6]
dane <- ts(dane)
#wykres przebiegu szeregu
ts.plot(dane)

n <- length(dane)
Ntr <- n-5
Nts <- 5
znext <- ts(dane[(Ntr+1):n],start=Ntr+1,end=n)
zt <- ts(dane[1:Ntr],start=1,end=Ntr)

ts.plot(zt)
#c
acf(zt)
pacf(zt)
#d
z_diff <- diff(zt)
plot(z_diff,type="l")

acf(z_diff)
#mo¿e MA(0)
pacf(z_diff)
#mo¿e AR(0)

#e
#Ponizej znajduje sie dopasowanie modelu AR przy uzyciu funkcji ar.
model_ar <- ar(z_diff,order.max=NULL,aic=TRUE,method="mle")
model_ar
#Z wyniku widzimy, ze jest to model AR(0). Ponizej dopasowanie tego modelu za pomoca funkcji arima.
model_ar1<-arima(z_diff, order=c(0,0,0))

Box.test(model_ar1$resid, lag=10, type="Ljung") # bia³y szum

#f
aic <- matrix(0,6,6)
bic <- matrix(0,6,6)
aic_min <- Inf
bic_min <- Inf
i_aic_min <- -1
j_aic_min <- -1
i_bic_min <- -1
j_bic_min <- -1
for(i in 0:5){
  for(j in 0:5){
    cat(" \r")
    aic[i+1,j+1]<-AIC(arima(z_diff,c(i,0,j),method="ML",optim.control=list(maxit=10^5)))
    if(aic[i+1,j+1]<aic_min)
    {
      aic_min <- aic[i+1,j+1]
      i_aic_min <- i
      j_aic_min <- j
    }
    bic[i+1,j+1]<-AIC(arima(z_diff,c(i,0,j),method="ML",optim.control=list(maxit=10^5)),k=log(Ntr))
    if(bic[i+1,j+1]<bic_min)
    {
      bic_min <- bic[i+1,j+1]
      i_bic_min <- i
      j_bic_min <- j
    }
  }
}

i_aic_min
j_aic_min
i_bic_min
j_bic_min

#Z otrzymanych powyzej wyników widzimy, ze kryterium AIC sugeruje model ARIMA(0,1,0), natomiast
#kryterium BIC sugeruje model ARIMA(0,1,0).

#Ponizej zbada³am jak zachowuja sie rezydua w tych obu modelach.
arma_AIC <- arima(z_diff,c(0,0,0))
arma_BIC <- arima(z_diff,c(0,0,0))
Box.test(arma_AIC$resid, lag=10, type="Ljung")
Box.test(arma_BIC$resid, lag=10, type="Ljung")

#Widzimy, ze dla obu modeli nie ma podstaw do odrzucenia hipotezy zerowej mówiacej o niezaleznosci
#rezyduów.
#Ponizej znajduje sie predykcja 5 wartosci oraz wykres dla 95% przedzia³ów ufnosci.

n.ahead<-5
predAIC<-predict(arma_AIC,n.ahead=n.ahead)$pred
seAIC<-predict(arma_AIC,n.ahead=n.ahead)$se
plot(NULL,xlim=c(0.98*Ntr,n),ylim=c(-70,70),xlab="t",ylab="z_diff",main="Predykcja AIC")
lines((0.98*Ntr):(Ntr-1),z_diff[(0.98*Ntr):(Ntr-1)],lty=1,col="black")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predAIC),lty=3,col="black")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predAIC+2*seAIC),lty=3,col="red")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predAIC-2*seAIC),lty=3,col="red")

predBIC<-predict(arma_BIC,n.ahead=n.ahead)$pred
seBIC<-predict(arma_BIC,n.ahead=n.ahead)$se
plot(NULL,xlim=c(0.98*Ntr,n),ylim=c(-70,70),xlab="t",ylab="z_diff",main="Predykcja BIC")
lines((0.98*Ntr):(Ntr-1),z_diff[(0.98*Ntr):(Ntr-1)],lty=1,col="black")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predBIC),lty=3,col="black")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predBIC+2*seBIC),lty=3,col="red")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],predBIC-2*seBIC),lty=3,col="red")


#kolejne 10 lat

n.ahead<-5+10
predAIC<-predict(arma_AIC,n.ahead=n.ahead)$pred
seAIC<-predict(arma_AIC,n.ahead=n.ahead)$se
plot(NULL,xlim=c(0.98*Ntr,n+10),ylim=c(-70,70),xlab="t",ylab="z_diff",main="Predykcja AIC")
lines((0.98*Ntr):(Ntr-1),z_diff[(0.98*Ntr):(Ntr-1)],lty=1,col="black")
lines((Ntr-1):(n-1),c(z_diff[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue")
lines((Ntr-1):(n-1+10),c(z_diff[Ntr-1],predAIC),lty=3,col="black")
lines((Ntr-1):(n-1+10),c(z_diff[Ntr-1],predAIC+2*seAIC),lty=3,col="red")
lines((Ntr-1):(n-1+10),c(z_diff[Ntr-1],predAIC-2*seAIC),lty=3,col="red")

ostatnia <- znext[5]
notowanie <- numeric(10)
for(i  in 1:10){
notowanie[i] <- ostatnia + predAIC[5+i]
ostatnia <- notowanie[i]
}
#wyniki dla kolejnych 10-ciu lat
notowanie

