#zad.1

x<-sin(seq(0,8*pi,0.01))+cos(5*seq(0,8*pi,0.01))
x<-ts(x,frequency=100)
plot(x,type="l")
S<-spectrum(x,ci=0)
S$freq[which.max(S$spec)]
1/(S$freq[order(-S$spec)[1:2]])
c(2*pi,2/5*pi)

#zad. 2
#a
library(MASS)
data(co2)
co2d<-diff(co2)
plot(co2)
plot(co2d)
spectrum(co2)
spectrum(co2d)
Pco2<-spectrum(co2d,ci.col="red")
Pco2$freq[which.max(Pco2$spec)]

cpgram(co2d)#skumulowany periodogram

#b
co2dec<-decompose(co2)
co2dec
plot(co2dec)
#c
Pco2_S<-spectrum(co2dec$seasonal)
Pco2_S$freq[which.max(Pco2_S$spec)]
cpgram(co2dec$seasonal)

co2_random<-window(co2dec$random,start=c(1959,7),end=c(1997,6))
Pco2_R<-spectrum(co2_random)#stosujemy window, ponieważ na poczatku i koncu mamy wartosci NA
#wynika to z dzialania funkcji decompose
Pco2_R<-spectrum(co2_random,c(5,5))#wyg�adzanie
1/Pco2_R$freq[which.max(Pco2_R$spec)]# okres 0.9756 roku przypadkowy??
cpgram(co2_random)#wniosek - jeszcze mamy sezonowosc w komponencie losowej
acf(co2_random)#na tym wykresie również obserwujemy sezonowość
Box.test(co2_random,lag=21,type="Ljung")#czyli komponenta losowa nie jest białym szumem

#d
loc.max.coor<-function(x) {
   n<-length(x)
   max_loc<-which(x[-c(1,n)]>x[-c(n-1,n)]&x[-c(1,n)]>x[-c(1,2)])+1#+1, ponieważ R zmienia indeksy
   if(x[1]>x[2]){
      max_loc<-c(1,max_loc)
   }
   if(x[n]>x[n-1]){
      max_loc<-c(max_loc,n)
   }
   return (max_loc)
}

#loc.max.coor(c(0,3,2,6,0,-1,9,10,1))

Pco2_signif_freq<-Pco2$freq[loc.max.coor(Pco2$spec)]
Pco2_signif_freq[order(-Pco2$spec[loc.max.coor(Pco2$spec)])]#maksima lokalne
                                                          #sortowane wg Pco2$spec

#Prand=spectrum(window(decompose(co2)$random,start=c(1970,7),
#                      end=c(1997,6)))#musimy wyci?? braki danych - 6 z pocz?tku i 6 z ko?ca
#frequency(decompose(co2)$random)

#1/(Prand$freq[which.max(Prand$spec)]) # okres 0.9756 roku przypadkowy??
#cpgram(decompose(co2)$random)#czyli nie jest to bia?y szum


#e
?HoltWinters

m<-HoltWinters(co2,seasonal="additive")
m$coefficients
m$fitted
#lm(co2~c(1:length(co2)))

p <- predict(m, n.ahead=50, prediction.interval = TRUE)
plot(m, p)

#f
m <- HoltWinters(co2, gamma=FALSE)#bez cz??ci sezonowej, s?aba predykcja
p <- predict(m, n.ahead=50, prediction.interval = TRUE)
plot(m, p)
