dane<- read.csv2("metr.csv")
tau<-seq(0.1,1,0.01)
x <- dane$x
phi <- numeric(length(tau))
for (i in 1:length(phi)){
  phi[i] <- sum(cos(2*pi*tau[i]*x))
}
q <- 1/tau[which.max(phi)]
q





