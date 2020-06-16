#6.1

#a
alfa<-c(0.1,0.25,0.2)
z<-rnorm(1100,0,1)
x<-numeric(1101)
x[1:2]<-rnorm(2,0,sqrt(alfa[1]/(1-alfa[2]-alfa[3])))
for(i in 3:1101){
  x[i]<-z[i-1]*sqrt((alfa[1]+alfa[2]*x[i-1]^2+alfa[3]*x[i-2]^2))
}
x<-x[102:1101]
x<-as.ts(x)
ts.plot(x)
#b
Box.test(x,type="Ljung",lag=32)
acf(x)
pacf(x)
Box.test(x^2,type="Ljung",lag=32)
acf(x^2)
pacf(x^2)

#c
#mozemy zobaczyc, jaki rzad dopasuje funkcja ar
modelar<-ar(x^2,method="mle")
modelar

#a teraz bierzemy prawdziwy rzad 2
mar<-arima(x^2,order=c(2,0,0))
mar

Box.test(mar$res,type="Ljung",lag=32)

library(tseries)

x.arch<-garch(x,order=c(0,2),trace=FALSE)
summary(x.arch)$coef
alfa

#e
logLik(x.arch)
AIC(x.arch)
plot(x.arch)# wykresy diagnostyczne 

#f
x.fitted<-fitted(x.arch)#daje wyestymowane sigma
plot(x.fitted[,1]^2,type="l",ylab="Cond. Variance")

#g

x.garch<-garch(x,order=c(1,1),trace=FALSE)

summary(x.garch)
#test Jarque Bera daje nam normalnosc Z_t - stosujemy tu ten test zamiast Shapiro - Wilka, 
#poniewaz licznosc proby jest duza i obliczenie dla testu Shapiro w tej sytuacji wykonywaloby sie zbyt dlugo
#z testu Ljunga-Boxa mamy, ze rezydua sa bialym szumem

# h)
logLik(x.garch)

AIC(x.garch,k=log(1000))
AIC(x.arch, k=log(1000))#ARCH ma nizsza wartosc kryterium AIC

#9.2
zwrot<-read.table("exch1.txt")
par(mfrow=c(1,1))
zwrot<-ts(zwrot)
plot(zwrot)
#a
acf(zwrot)
pacf(zwrot)

acf(zwrot^2)
pacf(zwrot^2)

Box.test(zwrot^2,lag=20,type="Ljung")

#b
plot(density(zwrot))
curve(dnorm(x,mean(zwrot),sd(zwrot)),add=TRUE,col="red")

#obciecie do (-0.25,0.25)
plot(density(zwrot),xlim=c(-0.25,0.25))
curve(dnorm(x,mean(zwrot),sd(zwrot)),add=TRUE,col="red")

#ARCH(3) - probujemy na podstawie wykresu PACF dla zmiennej zwrot^2
#c)
zwrot.arch<-garch(zwrot,c(0,3),trace=FALSE)
summary(zwrot.arch)#a wiec rezydua sa bialym szumem, ale nie o rozkladzie normalnym

#d) dopasowujemy GARCH(1,1)

zwrot.garch <- garch(zwrot,order=c(1,1),trace=FALSE)
summary(zwrot.garch)
plot(zwrot.garch)
AIC(zwrot.arch)
AIC(zwrot.garch) # GARCH lepszy

#e) dob?r modelu kryterium BIC

for (i in 0:3) {
  for (j in 1:3) {
    a=AIC(garch(zwrot,order=c(i,j),trace=FALSE),k=log(length(zwrot)))
    print(c(i,j,a))
  }}

#(2,1)


#zadanie 3

# O nieadekwatnosci modelowania zwrotow na
# podstawie liniowych modeli autoregresyjnych

#install.packages("evir")
library(evir)

data(bmw,package="evir")
#bmw - wektor zwrotow logarytmicznych
bmw = as.vector(bmw)
n=length(bmw)
#n=6146 
par(mfrow=c(2,2))
plot(acf(bmw)$acf[-1])

pacf(bmw)
plot(bmw,type="l")
#a)
for (p in 0:3) {
  for (q in 0:3) {
    a=AIC(arima(bmw,c(p,0,q)),k=log(n))
    print(c(p,q,a))
  }}
# sugeruje model MA(1)

fitMA1 = arima(bmw, order = c(0,0, 1))
#b)
Box.test(fitMA1$resid,lag=78,type="Ljung")
Box.test(fitMA1$resid^2,lag=78,type="Ljung")  # wskazuje na szereg ARCH


acf( residuals(fitMA1),lag.max=20 , main="")
qqnorm(residuals(fitMA1),datax=T,main="MA(1) resid")
plot(residuals(fitMA1),ylab="Residual")

#rozklad rezyduow nie jest normalny
# widoczne skupienia zmiennosci => rezydua sa zalezne.


#### dopasowujemy model MA(1) + rezydua GARCH(1,1), rozklad warunkowy normalny
#c)
library(fGarch)

bmw.garch_norm = garchFit(~arma(0,1)+garch(1,1),data=bmw,cond.dist="norm",trace=FALSE)

summary(bmw.garch_norm)

# wykres kwantylowy dla rezyduow

x = bmw.garch_norm@residuals / bmw.garch_norm@sigma.t
plot(x) ## 
qqnorm(x,datax=T,ylab= "Standardized residual quantiles",
       main=" normal plot", xlab="normal quantiles")
qqline(x,datax=T)

# duze odstepstwa od normalnosci - grubsze ogony.
# Dopasowujemy rozklad t do rezyduow.
# Wykonujemy qqplot  dla rozkladu t o 4 st. sw.
grid = (1:n)/(n+1)
qqplot(sort(x), qt(grid,df=4),
       main= " t plot, df=4",xlab= "Standardized residual quantiles",
       ylab="t-quantiles")
abline(   lm(   qt(c(.25,.75),df=4)~quantile(x,c(.25,.75))   )   )


?garchFit
# zmieniamy rozklad warunkoowy na t
###
bmw.garch_t = garchFit(~arma(0,1)+garch(1,1),cond.dist="std",data=bmw)

options(digits=4)
summary(bmw.garch_t)  # parametr shape-> stopnie swobody.
acf(bmw.garch_t@residuals / bmw.garch_t@sigma.t)
loglik_bmw=bmw.garch_t@fit$llh # -loglik dla modelu bmw.garch_t


BIC_bmw_t=2*loglik_bmw+log(n)*6#-2log. wiar.+log(n)*liczba par.
as.numeric(BIC_bmw_t) # wartoA›Ä‡ kryterium BIC dla tego modelu

AIC(fitMA1,k=log(n))

#6.4
x<-numeric(1100)
alfa = c(0.1, 0.1, 0.3)
beta = 0.2
x[1:2]<-rnorm(2,0,1)
sigma<-numeric(1100)
sigma[1:2]<-(rnorm(2,0,1))^2
z<-rnorm(1100,0,1)
for(i in 3:1100){
  sigma[i]<-alfa[1]+alfa[2]*x[i-1]^2+alfa[3]*x[i-2]^2+beta*sigma[i-1]
  x[i]<-sqrt(sigma[i])*z[i]
}

library(tseries)
model<-garch(x,order=c(1,2),trace=FALSE)
model
summary(model)


try=dget("./szeregi/dane/try")

plot(try)

acf(try)

pacf(try)

try_d=diff(try)
plot(try_d)

acf(try_d) 

pacf(try_d)

try_2d=diff(try_d)

plot(try_2d)

acf(try_2d) 

pacf(try_2d)

Druga grupa: takie samo zadanie z tego procesu generowanego z innym parametrem

2) Zadanie 2 z kolokwium poprawkowego z zesz3ego roku


3) Inna propozycja

#Wygeneruj proces ARMA(2,2)  z \phi(x)=1-0.5x +0.5x^2 i \theta(x)=1 + 0.8x +0.2x^2

arma_test=arima.sim(n=300,list(ar=c(0.5,-0.5),ma=c(0.8,0.2),sd=1))

plot(arma_test)

# Dopasuj du??y model np ARMA(4,4). Czy wspólczynniki zgdzaja sie z wartoociami teoretycznymi ? Dlaczego ?

arima(arma_test,c(4,0,4))


#Wybierz najlepszy model arima metoda AIC i BIC z zakresami parametrow 0,6 i 0,6

n=300

aic<-matrix(0,6,6)
bic<-matrix(0,6,6)

aic_min<-Inf
bic_min<-Inf
i_aic_min<--1
j_aic_min<--1
i_bic_min<--1
j_bic_min<--1
for(i in 0:5){
  for(j in 0:5){
    cat("                      \r")
    cat("i=",i,", j=",j)
    aic[i+1,j+1]<-AIC(arima(arma_test,c(i,0,j),method="ML",
                            optim.control=list(maxit=10^5)))
    if(aic[i+1,j+1]<aic_min){
      aic_min<-aic[i+1,j+1]
      i_aic_min<-i
      j_aic_min<-j
    }
    bic[i+1,j+1]<-AIC(arima(arma_test,c(i,0,j),method="ML",
                            optim.control=list(maxit=10^5)),k=log(n))
    if(bic[i+1,j+1]<bic_min){
      bic_min<-bic[i+1,j+1]
      i_bic_min<-i
      j_bic_min<-j
    }
  }
  
} 


i_aic_min
j_bic_min

# dopasuj wybrany model. Skomentuj wyniki