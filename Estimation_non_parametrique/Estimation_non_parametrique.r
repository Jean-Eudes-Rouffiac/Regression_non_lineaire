# ----------------------------------------------------
#TP5 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------




###Exercice 1

##Partie 1
F = function(X){
  F = (X*X-1)
}

resid<-matrix(nrow=121,ncol=4)

#Question 3
t <- seq(-3,3,0.05)


par(mfrow = c(2,2))
for (n in c(100,200,500,1000)) {
  X<-rnorm(n,mean=0,sd=1)
  e<-rnorm(n,mean=0,sd=0.25)
  
  Y=F(X)+e
  
  ks<-ksmooth(X,Y,kernel="normal",x.point= t)
  
  plot(F,xlim=c(-3,3),type="l",main = paste("Nombre de données = " , n), col="black")
  lines(ks,type="l",col=2)
  legend("top",legend=c("Fonction","Estimation"), col=c("black","red"),pch=15, bty="n",cex=0.8)
  
}



#Question 2
n=1000
X<-rnorm(n,mean=0,sd=1)
e<-rnorm(n,mean=0,sd=0.25)
Y=F(X)+e

v<-sd(X)
i=1

par(mfrow = c(2,2))
for (alpha in c(0.05,0.2,0.5,0.8)) {
  
  ks<-ksmooth(X, Y, kernel="normal", x.point= t, bandwidth=v*n^(-alpha))
  resid[,i]<-ks$y-F(t)
  plot(F,xlim=c(-3,3),type="l",main = paste("alpha = " , alpha), col="black")
  lines(ks,type="l",col=2)
  legend("top",legend=c("Fonction","Estimation"), col=c("black","red"),pch=15, bty="n",cex=0.8)
  i=i+1
}



par(mfrow = c(1,1))
n=1000
s=5000
ks=matrix(nrow=121,ncol=s)
for (i in 1:s) {
  X<-rnorm(n,mean=0,sd=1)
  e<-rnorm(n,mean=0,sd=0.25)
  Y=F(X)+e
  ks[,i]=ksmooth(X, Y, kernel="normal", x.point= t)$y
}


bandes=apply(as.matrix(ks),1,quantile,c(0.025,0.975))

plot(x=t,bandes[1,],col="red",cex=1.5, cex.lab=1.5, font=2, font.lab=2, type="l", xlab="x", ylab="y", main = "Bande de confiance 5%")
lines(x=t,bandes[2,],col="red",cex=1.5, cex.lab=1.5, font=2, font.lab=2, type="l")
#lines(x=t,F(t))


#Residus
res <- ksmooth(X,Y,x.point = X)
eps_chap=numeric(n)
ind = order(X)

for (i in 1:n) {
  ind_i = ind[i]
  eps_chap[ind_i] <- Y[ind_i] - res$y[i]
}
par(mfrow = c(1,1))
plot(eps_chap,font.lab=2,pch=19,ylab="Résidus", main="Graphe des résidus")
abline(h=0,lwd=2,col=2)
shapiro.test(eps_chap)
sd(eps_chap)


#hist(eps_chap, col="cyan" , main="Histogramme des résidus", cex=1.5, cex.main = 1.7,cex.axis=1.5, xlab="Résidus")






##Partie 2
F = function(X){
   F = 0.2*X^11*(10*(1-X))^6+10*(10*X)^3*(1-X)^10
}

#Question 2
n=1000
X<-runif(n,min=0,max=1)
e<-rnorm(n,mean=0,sd=1)
Y=F(X)+e

v<-sd(X)

par(mfrow = c(2,2))
for (alpha in c(0.05,0.2,0.5,0.8)) {
  
  ks<-ksmooth(X, Y, kernel="normal", x.point= t, bandwidth=v*n^(-alpha))
  
  plot(F,xlim=c(0,1),type="l",main = paste("alpha = " , alpha), col="black")
  lines(ks,type="l",col=2)
  legend("topright",legend=c("Fonction","Estimation"), col=c("black","red"),pch=15, bty="n",cex=0.8)
  
}



#Question 3
t <- seq(0,1,0.01)
alpha=0.2

par(mfrow = c(2,2))
for (n in c(100,200,500,1000)) {
   X<-runif(n,min=0,max=1)
   e<-rnorm(n,mean=0,sd=1)
   Y=F(X)+e
   
   
   
   ks<-ksmooth(X,Y,kernel="normal",x.point= t,bandwidth=v*n^(-alpha))
   
   
   plot(F,xlim=c(0,1),type="l",main = paste("Nombre de données = " , n), col="black")
   lines(ks,type="l",col=2)
   legend("topright",legend=c("Fonction","Estimation"), col=c("black","red"),pch=15, bty="n",cex=0.8)
   #lines(x = t, F(t),type="l",col="red")
   
}






n=1000
s=2000
alpha=0.2
ks=matrix(nrow=101,ncol=s)
for (i in 1:s) {
   X<-runif(n,min=0,max=1)
   e<-rnorm(n,mean=0,sd=1)
   Y=F(X)+e
   ks[,i]=ksmooth(X, Y, kernel="normal", x.point= t,bandwidth=v*n^(-alpha))$y
}

par(mfrow = c(1,1))
bandes=apply(as.matrix(ks),1,quantile,c(0.025,0.975))

plot(x=t,bandes[1,],col="red",cex=1.5, cex.lab=1.5, font=2, font.lab=2, type="l", xlab="x", ylab="y", main = "Bande de confiance 5%")
lines(x=t,bandes[2,],col="red",cex=1.5, cex.lab=1.5, font=2, font.lab=2, type="l")
#lines(x=t,F(t))


#Residus
n=1000
alpha=0.2
X<-runif(n,min=0,max=1)
e<-rnorm(n,mean=0,sd=1)
Y=F(X)+e
res <- ksmooth(X,Y,x.point = X,bandwidth=v*n^(-alpha))
eps_chap=numeric(n)
ind = order(X)

for (i in 1:n) {
  ind_i = ind[i]
  eps_chap[ind_i] <- Y[ind_i] - res$y[i]
}
plot(eps_chap,font.lab=2,pch=19,ylab="Résidus", main="Graphe des résidus")
abline(h=0,lwd=2,col=2)

shapiro.test(eps_chap)
sd(eps_chap)











###Exercice 2

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

v=sd(T)

#Fenetre automatique
res3=ksmooth(dataApp$Temperature, dataApp$TauxCroissance, kernel="normal",x.points = dataApp$Temperature)
plot(dataApp,type="p",main = "Estimateur de Nadaraya-Watson")
lines(ksmooth(dataApp$Temperature, dataApp$TauxCroissance, kernel="normal",x.points = dataApp$Temperature),type="l",col="red")
#calcul des residus
eps_chap3=numeric(n)
ind3 = order(dataApp$Temperature)
for (i in 1:n) {
  ind_i = ind3[i]
  eps_chap3[ind_i] <- dataApp$TauxCroissance[ind_i] - res3$y[i]
}
SCR3=sum((eps_chap3-mean(eps_chap3))^2)
SCR3
sd(eps_chap3)





#Fenetre avec sd*n^-0.2
n=184
alpha=0.2
res2 <- ksmooth(dataApp$Temperature, dataApp$TauxCroissance, x.point = dataApp$Temperature,bandwidth=v*n^(-alpha))
plot(dataApp,type="p",main = "Estimateur de Nadaraya-Watson")
lines(res2,type="l",col="red")
#calcul des residus
eps_chap2=numeric(n)
ind2 = order(dataApp$Temperature)
for (i in 1:n) {
  ind_i = ind2[i]
  eps_chap2[ind_i] <- dataApp$TauxCroissance[ind_i] - res2$y[i]
}
SCR2=sum((eps_chap2-mean(eps_chap2))^2)
SCR2
sd(eps_chap2)



#On cherche la fenêtre à la main par validation croisée
n=184
EQM=matrix(nrow=n)
i=0
for (h in seq(0, 8, 0.01)){
ks<-ksmooth(dataApp$Temperature, dataApp$TauxCroissance, kernel="normal",x.points = dataApp$Temperature, bandwidth=h)
#plot(dataApp,type="p",main = "Estimateur de Nadaraya-Watson")
#lines(ks,type="l",col=2)
x=ks$y 
diff=Y-x
EQM[i]=sum(diff*diff)/n
i=i+1
}
h_opt = which.min(EQM) * 0.01 #on a pris un pas de 0.01
h_opt #affichage de h optimal


res <- ksmooth(dataApp$Temperature, dataApp$TauxCroissance, x.point = dataApp$Temperature,bandwidth=h_opt)
plot(dataApp,type="p",main = "Estimateur de Nadaraya-Watson avec fenêtre 0.15")
lines(res,type="l",col=2)




#calcul des residus
eps_chap=numeric(n)
ind = order(dataApp$Temperature)
for (i in 1:n) {
  ind_i = ind[i]
  eps_chap[ind_i] <- dataApp$TauxCroissance[ind_i] - res$y[i]
}

#graphe des résidus
plot(ks$x,eps_chap ,font.lab=2,pch=19,ylab="Résidus", main="Residus pour l'esimateur de Nadaraya Watson", xlab="Température")
abline(h=0,lwd=2,col=2)


#graphe obs/Estim
plot(dataApp$TauxCroissance, res$y, main="Nuage de points (observé/estimé) ",xlab="Taux de croissance observé", ylab="Taux de croissance estimé")
abline(0,1,col=2)


SCR=sum((eps_chap-mean(eps_chap))^2)
SCR
sd(eps_chap)


#Bootstrap
sol=res$y
par(mfrow = c(1,1))
x0 = data.frame(T=seq(5,50,by=1))
Y_etoile <- c()

N=184
B= 1000
M = matrix(data=NA, nrow=B, ncol=N, byrow=TRUE)

ResNew <- c()

for ( b in 1:B ) {
  for ( k in 1:N ) {
    i<-sample(1:N,1,replace=TRUE)
    temp = mean(eps_chap,trim=0.001)
    ResNew[k]<-eps_chap[i] - temp
  }
  Yb<-sol+ResNew 
  
  #resul = ksmooth(dataApp$Temperature, Yb, x.point = dataApp$Temperature,bandwidth=h_opt)
  ks_etoile = ksmooth(dataApp$Temperature, Yb, kernel="normal",x.points = dataApp$Temperature, bandwidth=h_opt)
  M[b,] = ks_etoile$y
}


bande1=M[25,]
bande2=M[975,]
#bandes <- apply(M,2,quantile,c(0.025,0.975))



plot(dataApp$Temperature,dataApp$TauxCroissance,col="red",main="Bandes de confiance",xlab="Température",ylab="Taux de croissance")

lines(dataApp$Temperature, bande1,col="cyan",cex=1.5, cex.lab=1.5, font=2, font.lab=2,lw=2.2)
lines(dataApp$Temperature, bande2,col="cyan",cex=1.5, cex.lab=1.5, font=2, font.lab=2,lw=2.2)








