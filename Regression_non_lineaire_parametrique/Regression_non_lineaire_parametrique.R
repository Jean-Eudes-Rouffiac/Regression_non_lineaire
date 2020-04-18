# ----------------------------------------------------
#TP4 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------

barber=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/barber.txt",header=TRUE)
plot(barber)
set.seed(111) # initialisation du générateur
# Extraction des échantillons
test.ratio = 0.15 # part de l’échantillon test
npop = nrow(barber) # nombre de lignes dans les donn´ees
ntest = ceiling(npop*test.ratio) # taille de l’échantillon test
testi = sample(1:npop,ntest) # indices de l’échantillon test
appri = setdiff(1:npop,testi) # indices de l’échant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives .
dataApp = barber[appri,] # construction de l’échantillon d’apprentissage
dataTest = barber[testi,] # construction de l’échantillon test

Y <- dataApp[["TauxCroissance"]]
T <- dataApp[["Temperature"]]

jpeg("~/Desktop/TP4/Moi/1.jpg ")
plot(dataApp)
dev.off()

F = function(T,Tmin,Tmax,Topt,Yopt){
  Ind = 0*T
  Ind[(T >= Tmin)&(T<=Tmax)] = 1
  F = Yopt*(T-Tmax)*(T-Tmin)^2 / ((Topt-Tmin)*((Topt-Tmin)*
                                                 (T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T)))*Ind
}
resnls = nls(Y~F(T,Tmin,Tmax,Topt,Yopt),start=c(Tmin=3 ,Tmax=50 ,Topt=40,Yopt= 2.4),data = dataApp)

summary(resnls)

plot(T,Y,main="Développement des bactéries",xlab="Température", ylab="Taux de croissance",pch=19,cex=1,cex.lab=1.5,font=2,font.lab=2)
lines(T,predict(resnls),lwd=2,col=2)

par(mfrow = c(1,1))

plot(T,residuals(resnls),xlab="Température",ylab="Residus",cex=1,cex.lab=1.5,font=2,font.lab=2, pch=19)
abline(h=0,lwd=2,col=2)



plot(Y,predict(resnls),xlab="Taux de croissance observé",ylab="Taux de croissance predit",pch=19,cex=1,cex.lab=1.5,font=2,font.lab=2)
abline(0,1,lwd=2,col=2)





#Avec echantillon test
Y2 <- dataTest[["TauxCroissance"]]
T2 <- dataTest[["Temperature"]]

x = predict(resnls, newdata=list(T=as.numeric(T2)))
sort(x)
plot(dataTest)
par(mfrow = c(1,2))
plot(T2,Y2,main="Développement des bactéries",xlab="Température", ylab="Taux de croissance",pch=19,cex=1,cex.lab=1.5,font=2,font.lab=2)
lines(sort(dataTest$Temperature), predict(resnls, newdata = list(T = sort(dataTest$Temperature))),lwd=2,col=2)

R= Y2-predict(resnls, newdata=list(T=as.numeric(T2)))

plot(Y2,x,xlab="Taux de croissance observé",ylab="Taux de croissance predit",pch=19,cex=1,cex.lab=1.5,font=2,font.lab=2)
abline(0,1,lwd=2,col=2)

plot(T2 ,R,xlab="Température",ylab="Residus",cex=1,cex.lab=1.5,font=2,font.lab=2, pch=19)
abline(h=0,lwd=2,col=2)



rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}

error <- Y2 - x
rmse(error)
mae(error)


#Bootstrap
R=residuals(resnls)
sol<-F(T,coefficients(resnls)[1],coefficients(resnls)[2],coefficients(resnls)[3],coefficients(resnls)[4])
B=1000
N=184

theta=matrix(nrow=4,ncol=B)
x=numeric(N)

x0=seq(5,50,1)
Y_etoile=c()

for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    x[k]<-R[i]
  }
  Yb<-sol+x
  
  resnlstest = nls(Yb~F(T,Tmin,Tmax,Topt,Yopt),start=c(Tmin=0,Tmax=52,Topt= 40,Yopt=2.5))
  theta[,b]=coefficients(resnlstest)
  
  Y_etoile=cbind(Y_etoile,predict(resnlstest,x0))
  
}

meanparam=c(mean(theta[1,]),mean(theta[2,]),mean(theta[3,]),mean(theta[4,]))

#Erreurs standard bootstrap

SE1=sqrt(1/(B-1)*sum((theta[1,]-mean(theta[1,]))^2))
SE1
SE2=sqrt(1/(B-1)*sum((theta[2,]-mean(theta[2,]))^2))
SE2
SE3=sqrt(1/(B-1)*sum((theta[3,]-mean(theta[3,]))^2))
SE3
SE4=sqrt(1/(B-1)*sum((theta[4,]-mean(theta[4,]))^2))
SE4

par(mfrow = c(2,2))
hist(theta[1,],main="",ylab="Fréquence",xlab="Tmin",cex=1,cex.lab=1.5,font=2,font.lab=2, col="cyan")
hist(theta[2,],main="",ylab="Fréquence",xlab="Tmax",cex=1,cex.lab=1.5,font=2,font.lab=2, col="cyan")
hist(theta[3,],main="",ylab="Fréquence",xlab="Topt",cex=1,cex.lab=1.5,font=2,font.lab=2, col="cyan")
hist(theta[4,],main="",ylab="Fréquence",xlab="Yopt",cex=1,cex.lab=1.5,font=2,font.lab=2, col="cyan")


#Intervalles conf percentiles
perc<-sort(theta[1,],decreasing=FALSE)
c(perc[trunc(B*0.05/2)],perc[trunc(B*(1-0.05/2))])

perc<-sort(theta[2,],decreasing=FALSE)
c(perc[trunc(B*0.05/2)],perc[trunc(B*(1-0.05/2))])

perc<-sort(theta[3,],decreasing=FALSE)
c(perc[trunc(B*0.05/2)],perc[trunc(B*(1-0.05/2))])

perc<-sort(theta[4,],decreasing=FALSE)
c(perc[trunc(B*0.05/2)],perc[trunc(B*(1-0.05/2))])



par(mfrow = c(1,1))
x0 = data.frame(T=seq(5,50,by=1))
Y_etoile <- c()
N=184
B= 1000

ResNew <- c()

for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    temp = mean(R,trim=0.001)
    ResNew[k]<-R[i] - temp
  }
  Yb<-sol+ResNew 
  
  resnlstest = nls(Yb~F(T,Tmin,Tmax,Topt,Yopt),start=c(Tmin=5,Tmax=50,Topt= 39,Yopt=2.47))
  Y_etoile <- cbind(Y_etoile,predict(resnlstest,newdata=x0))
}

bandes <- apply(as.matrix(Y_etoile),1,quantile,c(0.025,0.975))

#temp1 = nls(Yb~F(T,Tmin,Tmax,Topt,Yopt),start=c(Tmin=mean(theta[1,]),Tmax=mean(theta[2,]),Topt=mean(theta[3,]),Yopt=mean(theta[4,])))
temp1 = nls(Y~F(T,Tmin,Tmax,Topt,Yopt),start=c(Tmin=mean(theta[1,]),Tmax=mean(theta[2,]),Topt=mean(theta[3,]),Yopt=mean(theta[4,])))
tempDot <- predict(temp1,x0) 

plot(tempDot,pch=18,col="red",main="Bandes de confiance",xlab="Température",ylab="Taux de croissance")
lines(bandes[1,],col="cyan",cex=1.5, cex.lab=1.5, font=2, font.lab=2,lw=2.2)
lines(bandes[2,],col="cyan",cex=1.5, cex.lab=1.5, font=2, font.lab=2,lw=2.2)

