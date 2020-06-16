#haczyk: model ARMA zró¿nicowany bedzie mia³ parametry,trzeba umiejêtnie je zinterpretowaæ w odniesieniu do oryginalnego
#zad1
szereg <- read.table("szereg.txt")
szereg <- ts(szereg)
ts.plot(szereg)
diff_s <- diff(szereg)
ts.plot(diff_s)
#b znika, at staje siê sta³¹
model1 <- arima(diff_s,c(1,0,3),include.mean=TRUE,method="ML")
summary(model1)
a <- model1$coef[5]
phi1 <- model1$coef[1]
theta1 <- model1$coef[2]+1
theta2 <- -model1$coef[4]
theta2 <- model1$coef[3]+theta1
t <- 1:1000
b <- mean(szereg-a*t)

#model lin

model2 <- lm(szereg~t)
ts.plot(szereg)
abline(model2,col="red")
summary(model2)
#reziduum to moje U_t
u <- szereg-model2$coef[1]-model2$coef[2]*t
plot(u)
#U_t jest tak¹jakby ARMA(1,2)
model3 <- arima(u,c(1,0,2),method="ML",include.mean=TRUE)

#zad2
library(quantmod)
dane2 <- read.table("LACounty.txt",h=T)
plot(dane2$TEMP,dane2$CARDIO)
m1 <- lm(CARDIO~DATE+I(TEMP-mean(TEMP))+I((TEMP-mean(TEMP))^2)+PART,data=dane2)
summary(m1)
acf(m1$res)
pacf(m1$res)
#sugeruj¹ ¿e nie s¹ bia³y szumem
Box.test(m1$res,lag=23,type="Ljung")
#odrzucamy
#jak naprawiæ taki model????

#najpierw dopasowujemy model AR
ar3 <- ar(m1$res)
#AR(3)
Box.test(ar3$resid,lag=23,type="Ljung",fitdf = 3)
#fitdf=3 bo trzeba skorygowac stopnie swobody dla nieznanych parametrów

Psi <- function(x, ar_coef){
  p <- length(ar_coef)
  w <- x - Lag(x,1:p) %*% ar_coef
  return(w)
}
t1 <- dane2$TEMP-mean(dane2$TEMP)
t2 <- t1^2
p <- dane2$PART
t <- dane2$DATE
y <- dane2$CARDIO
df <- data.frame(y,t,t1,t2,p)
df2 <- data.frame(apply(df,2,Psi,ar3$ar))
colnames(df2) <- paste0("N",colnames(df))
mod2<-lm(Ny~., data = df2)
summary(mod2)
acf(mod2$res)
pacf(mod2$res)
Box.test(mod2$res,lag=23,type="Ljung")
#no i co teraz? dalej zle , jeszcze raz powtórzyæ postêpowanie

ar2 <- ar(mod2$res)
#AR(2)
Box.test(ar2$res,lag=23,type="Ljung",fitdf=2)
df3 <- data.frame(apply(df2,2,Psi,ar2$ar))
colnames(df3) <- paste0("N",colnames(df2))
mod3 <- lm(NNy~.,data=df3)
summary(mod3)
acf(mod3$res)
pacf(mod3$res)
#xDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
