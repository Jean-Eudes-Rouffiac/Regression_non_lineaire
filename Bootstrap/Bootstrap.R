# ----------------------------------------------------
#TP2 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------

sample(1:20,20)
sample(1:20,20,replace=TRUE)

#Partie 1

carbone=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/carbone.txt",header=TRUE)


plot(carbone,main="Teneur en carbone en fonction de la résistance à la traction")

attach(carbone)
model<-lm(C~R)
theta<-cor(C,R) #coeff coefficient de corrélation linéaire
model$coefficients
plot(carbone,main="Teneur en carbone en fonction de la résistance à la traction", cex=1, cex.lab=1, cex.main =
       1,cex.axis=1)
abline(-32.37,1.09881, col=2, lwd=2)


test=100


r=0
B=1500
N=16
c=numeric(B)
x=matrix(nrow=N,ncol=2)
for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    x[k,1]<-carbone$R[i]
    x[k,2]<-carbone$C[i]
  }
  
  c[b]<-cor(x[,1],x[,2])
}
r=mean(c)-theta
r
SE=sqrt(1/(B-1)*sum((c-mean(c))*(c-mean(c))))
SE

boxplot(c,col="orange",main="Boîte à moustaches corrélation Bootstrap")
hist(c, col="orange",proba=T,xlab="Corrélation", main="Fréquence des corrélations calculées avec le rééchantillonnage Bootstrap", ylab="Fréquence", cex.main=0.8,ylim=c(1,450))
lines(density(c),lwd=2)
#Partie 2

systolique=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/systolique.txt",header=TRUE)

attach(systolique)
plot(systolique)
model<-lm(Tension~Age)
summary(model)
plot(systolique,main="Modélisation linéaire du jeu de données")
abline(-32.36876,1.09888, col=2, lwd = 2)

X=matrix(nrow=15,ncol=2)
X[,1]<-1
X[,2]<-systolique[,1]

summary(model)
res=resid(model)
theta = coefficients(model)

thetabis=matrix(nrow=2,ncol=1)
thetabis[1]=coefficients(model)[1]
thetabis[2]=coefficients(model)[2]



sol<-X%*%thetabis
age = 60
B=1500
N=15
c=numeric(B)

x=numeric(N)
est=numeric(B)
thetaetoile=matrix(nrow=2, ncol=B)
for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    x[k]<-res[i]
  }
  Y<-sol+x
  model2<-lm(Y~x)
  est[b]=coefficients(model2)[1]+coefficients(model2)[2]*age
  thetaetoile[,b]=solve(t(X)%*%X)%*%t(X)%*%Y
}

vec=matrix(nrow=1,ncol=2)
vec[1]=1
vec[2]=age

vecestimation=vec%*%thetaetoile

#Percentiles
perc<-sort(vecestimation,decreasing=FALSE)
perc[trunc(B*0.05/2)]
perc[trunc(B*(1-0.05/2))]

hist(sort(resid(model),decreasing=FALSE), main="Histogra")
library(car)
durbinWatsonTest(model)
shapiro.test(model$resid)

pred1 <- predict(model, interv="confidence")
confint(model)

#Partie 3

ozone=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/ozone.txt",header=TRUE)
attach(ozone)
plot(ozone)
model<-lm(maxO3~ T12 + Ne12 +Vx,ozone)

X=matrix(nrow=91,ncol=4)
X[,1]<-1
X[,2]<-ozone[,5]
X[,3]<-ozone[,10]
X[,4]<-ozone[,13]

summary(model)
res=resid(model)
theta = coefficients(model)

thetabis=matrix(nrow=4,ncol=1)
thetabis[1]=coefficients(model)[1]
thetabis[2]=coefficients(model)[2]
thetabis[3]=coefficients(model)[3]
thetabis[4]=coefficients(model)[4]

sol<-X%*%thetabis

B=1500
N=91
c=numeric(B)

x=numeric(N)
thetaetoile=matrix(nrow=4, ncol=B)
for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    x[k]<-res[i]
  }
  Y<-sol+x
  
  thetaetoile[,b]=solve(t(X)%*%X)%*%t(X)%*%Y
}

#Méthode ES
q=quantile(rnorm(100000),0.975)
##Premier paramètre
sigma<-sqrt(1/(B-1)*sum((thetaetoile[2,]-mean(thetaetoile[2,]))^2))
theta[2]-q*sigma
theta[2]+q*sigma
##2ème paramètre
sigma<-sqrt(1/(B-1)*sum((thetaetoile[3,]-mean(thetaetoile[3,]))^2))
theta[3]-q*sigma
theta[3]+q*sigma
##3ème paramètre
sigma<-sqrt(1/(B-1)*sum((thetaetoile[4,]-mean(thetaetoile[4,]))^2))
theta[4]-q*sigma
theta[4]+q*sigma
##mu
sigma<-sqrt(1/(B-1)*sum((thetaetoile[1,]-mean(thetaetoile[1,]))^2))
theta[1]-q*sigma
theta[1]+q*sigma

#Percentiles
perc<-sort(thetaetoile[2,],decreasing=FALSE)
perc[trunc(B*0.05/2)]
perc[trunc(B*(1-0.05/2))]

perc<-sort(thetaetoile[3,],decreasing=FALSE)
perc[trunc(B*0.05/2)]
perc[trunc(B*(1-0.05/2))]

perc<-sort(thetaetoile[4,],decreasing=FALSE)
perc[trunc(B*0.05/2)]
perc[trunc(B*(1-0.05/2))]

perc<-sort(thetaetoile[1,],decreasing=FALSE)
perc[trunc(B*0.05/2)]
perc[trunc(B*(1-0.05/2))]

pred1 <- predict(model, interv="confidence")
confint(model)

hist(sort(resid(model),decreasing=FALSE), main="Histogramme des r?sidus du mod?le",xlab="R?sidus",ylab="Fr?quence")
library(car)
durbinWatsonTest(model)
shapiro.test(model$resid)

biais=mean(thetaetoile[1,])-thetabis[1]

hist(thetaetoile[3,], col="orange",proba=T, main="Histogramme des estimateurs de la variable nébulosité ",xlab="Estimateurs",ylab="Fréquence")
lines(density(thetaetoile[3,]),lwd=2)
hist(thetaetoile[1,], col="orange",proba=T, main="Histogramme des estimateurs de la constante ",xlab="Estimateurs",ylab="Fréquence")
lines(density(thetaetoile[1,]),lwd=2)
hist(thetaetoile[2,], col="orange",proba=T, main="Histogramme des estimateurs de la variable température ",xlab="Estimateurs",ylab="Fréquence")
lines(density(thetaetoile[2,]),lwd=2)
hist(thetaetoile[4,], col="orange",proba=T, main="Histogramme des estimateurs de la variable projection du vent ",xlab="Estimateurs",ylab="Fréquence")
lines(density(thetaetoile[4,]),lwd=2)

help(lm)