# ----------------------------------------------------
#TP3 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------

#Partie 1 a)

n = 2000
u = runif(n)
x = rnorm(n)
Z = (u < 3/5) * (x-1) + (u> 3/5) * (x+2)


t<-seq(-5,6,by=0.1)

f = function(x){
  f = 3 * dnorm(x,-1,1) / 5 + 2 * dnorm(x,2,1) / 5
}

par(mfrow = c(2,2))

data=Z[1:50]
est1=density(data,kernel = "rectangular")
est2=density(data,kernel = "gaussian")
plot(est1, col="blue",main="N=50", ylim=c(0,0.25), xlab="")
lines(est2, col="red")
lines(t,f(t),col="black")


data=Z[1:100]
est1=density(data,kernel = "rectangular")
est2=density(data,kernel = "gaussian")
plot(est1, col="blue",main="N=100", ylim=c(0,0.25), xlab="")
lines(est2, col="red")
lines(t,f(t),col="black")


data=Z[1:500]
est1=density(data,kernel = "rectangular")
est2=density(data,kernel = "gaussian")
plot(est1, col="blue",main="N=500", ylim=c(0,0.25), xlab="")
lines(est2, col="red")
lines(t,f(t),col="black")


data=Z[1:2000]
est1=density(data,kernel = "rectangular")
est2=density(data,kernel = "gaussian")
plot(est1, col="blue",main="N=2000", ylim=c(0,0.25), xlab="")
lines(est2, col="red")
lines(t,f(t),col="black")

#Partie 1 b)
n = 2000
u = runif(n)
x = rnorm(n)
Z = (u < 3/5) * (x-1) + (u> 3/5) * (x+2)


t<-seq(-5,6,by=0.1)

f = function(x){
  f = 3 * dnorm(x,-1,1) / 5 + 2 * dnorm(x,2,1) / 5
}

Sz <- sqrt(mean(Z^2)-mean(Z)^2)

par(mfrow = c(2,2))

N=500
data=Z[1:N]
est=density(data,kernel = "gaussian",bw=Sz*N^(-0.4))
est0=density(data,kernel = "gaussian",bw=Sz*N^(-0.2))
est1=density(data,kernel = "gaussian",bw=Sz*N^(-0.15))
est2=density(data,kernel = "gaussian",bw=Sz*N^(-0.01))

plot(t,f(t), xlab="",ylab="",col="black", main="alpha=0.4",type="l",ylim=c(0,0.30))
lines(est, col="cyan")
plot(t,f(t), xlab="",ylab="",col="black", main="alpha=0.2",type="l",ylim=c(0,0.30))
lines(est0, col="cyan")
plot(t,f(t), xlab="",ylab="",col="black", main="alpha=0.15",type="l",ylim=c(0,0.30))
lines(est1, col="cyan")
plot(t,f(t), xlab="",ylab="",col="black", main="alpha=0.01",type="l",ylim=c(0,0.30))
lines(est2, col="cyan")


#Partie 2

N=500
donpol=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/donpol.txt",header=TRUE)
par(mfrow = c(1,3))
ozone=density(donpol[,3],kernel = "gaussian")
hist(donpol[,3], col="cyan",proba=T, xlab="Concentration en ozone", main="Histogramme en fréquence et densité estimée", cex.main=1,ylab="Fréquence",nclass=30)
lines(ozone,lwd=2)
ozone2=density(donpol[,3],kernel = "rectangular")
lines(ozone2,col="red")


temp=density(donpol[,1],kernel = "gaussian")
hist(donpol[,1], col="cyan",proba=T, xlab="Température", main="Histogramme en fréquence et densité estimée", cex.main=1,ylab="Fréquence",nclass=30)
lines(temp,lwd=2)
temp2=density(donpol[,1],kernel = "rectangular")
lines(temp2,col="red")


vent=density(donpol[,2],kernel = "gaussian")
hist(donpol[,2], col="cyan",proba=T, xlab="Vent", main="Histogramme en fréquence et densité estimée", cex.main=1,ylab="Fréquence",nclass=30)
lines(vent,lwd=2)
vent2=density(donpol[,2],kernel = "rectangular",bw=N^(-0.08))
lines(vent2,col="red")


shapiro.test(donpol[,1])
#shapiro.test(donpol[,2])
#shapiro.test(donpol[,3])



#Partie 3

N=500

n<-matrix(nrow=200,ncol=N)
shapn<-matrix(nrow=200)
kolgn<-matrix(nrow=200)
kolu1n<-matrix(nrow=200)
kolu2n<-matrix(nrow=200)

u<-matrix(nrow=200,ncol=N)
shapu<-matrix(nrow=200)
kolgu<-matrix(nrow=200)
kolu1u<-matrix(nrow=200)
kolu2u<-matrix(nrow=200)

s1<-matrix(nrow=200,ncol=N)
shaps1<-matrix(nrow=200)
kolgs1<-matrix(nrow=200)
kolu1s1<-matrix(nrow=200)
kolu2s1<-matrix(nrow=200)

s2<-matrix(nrow=200,ncol=N)
shaps2<-matrix(nrow=200)
kolgs2<-matrix(nrow=200)
kolu1s2<-matrix(nrow=200)
kolu2s2<-matrix(nrow=200)


for (i in 1:200) {
  n[i,]<-rnorm(N, mean=0, sd=1)
  u[i,]<-runif(N, -2,2)
  s1[i,]<-rt(N, df=5)
  s2[i,]<-rt(N, df=10)
}


cptshapn <- 0
cptkolgn <- 0
cptkolu1n <- 0
cptkolu2n <- 0

cptshapu <- 0
cptkolgu <- 0
cptkolu1u <- 0
cptkolu2u <- 0

cptshaps1 <- 0
cptkolgs1 <- 0
cptkolu1s1 <- 0
cptkolu2s1 <- 0

cptshaps2 <- 0
cptkolgs2 <- 0
cptkolu1s2 <- 0
cptkolu2s2 <- 0

for (i in 1:200) {
  shapn[i]=shapiro.test(n[i,])[[2]]
  if (shapn[i]>=0.05) {
    cptshapn = cptshapn +1
  }
  kolgn[i]=ks.test(n[i,],"pnorm")[[2]]
  if (kolgn[i]>=0.05) {
    cptkolgn = cptkolgn +1
  }
  kolu1n[i]=ks.test(n[i,],"punif",-1,1)[[2]]
  if (kolu1n[i]>=0.05) {
    cptkolu1n = cptkolu1n +1
  }
  kolu2n[i]=ks.test(n[i,],"punif",-2,2)[[2]]
  if (kolu2n[i]>=0.05) {
    cptkolu2n = cptkolu2n +1
  }
  
  shapu[i]=shapiro.test(u[i,])[[2]]
  if ( shapu[i]>=0.05) {
    cptshapu = cptshapu +1
  }
  kolgu[i]=ks.test(u[i,],"pnorm")[[2]]
  if (kolgu[i]>=0.05) {
    cptkolgu = cptkolgu +1
  }
  kolu1u[i]=ks.test(u[i,],"punif",-1,1)[[2]]
  if (kolu1u[i]>=0.05) {
    cptkolu1u = cptkolu1u +1
  }
  kolu2u[i]=ks.test(u[i,],"punif",-2,2)[[2]]
  if (kolu2u[i]>=0.05) {
    cptkolu2u = cptkolu2u +1
  }
  
  shaps1[i]=shapiro.test(s1[i,])[[2]]
  if ( shaps1[i]>=0.05) {
    cptshaps1 = cptshaps1 +1
  }
  kolgs1[i]=ks.test(s1[i,],"pnorm")[[2]]
  if (kolgs1[i]>=0.05) {
    cptkolgs1 = cptkolgs1 +1
  }
  kolu1s1[i]=ks.test(s1[i,],"punif",-1,1)[[2]]
  if (kolu1s1[i]>=0.05) {
    cptkolu1s1 = cptkolu1s1 +1
  }
  kolu2s1[i]=ks.test(s1[i,],"punif",-2,2)[[2]]
  if (kolu2s1[i]>=0.05) {
    cptkolu2s1 = cptkolu2s1 +1
  }
  
  shaps2[i]=shapiro.test(s2[i,])[[2]]
  if ( shaps2[i]>=0.05) {
    cptshaps2 = cptshaps2 +1
  }
  kolgs2[i]=ks.test(s2[i,],"pnorm")[[2]]
  if (kolgs2[i]>=0.05) {
    cptkolgs2 = cptkolgs2 +1
  }
  kolu1s2[i]=ks.test(s2[i,],"punif",-1,1)[[2]]
  if (kolu1s2[i]>=0.05) {
    cptkolu1s2 = cptkolu1s2 +1
  }
  kolu2s2[i]=ks.test(s2[i,],"punif",-2,2)[[2]]
  if (kolu2s2[i]>=0.05) {
    cptkolu2s2 = cptkolu2s2 +1
  }
}

###Gaussienne
print(c(cptshapn , cptkolgn , cptkolu1n , cptkolu2n))

###Uniforme
print(c(cptshapu , cptkolgu , cptkolu1u , cptkolu2u))

###Student 1
print(c(cptshaps1 , cptkolgs1 , cptkolu1s1 , cptkolu2s1))

###Student 2
print(c(cptshaps2 , cptkolgs2 , cptkolu1s2 , cptkolu2s2))


###Gaussienne
print(c(cptshapn/200 , cptkolgn/200 , cptkolu1n/200, cptkolu2n/200))

###Uniforme
print(c(cptshapu/200, cptkolgu/200, cptkolu1u/200, cptkolu2u/200))

###Student 1
print(c(cptshaps1/200 , cptkolgs1/200, cptkolu1s1/200, cptkolu2s1/200))

###Student 2
print(c(cptshaps2/200, cptkolgs2/200, cptkolu1s2/200, cptkolu2s2/200))