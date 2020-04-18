# ----------------------------------------------------
#TP6 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------


data=read.table("http://lmi2.insa-rouen.fr/~bportier/Data/p13.txt",header=TRUE)
plot(data,main="Nuages de points des données")
attach(data)
set.seed(111) # initialisation du générateur
test.ratio=.2 # part de l’échantillon test
npop=nrow(data) # nombre de lignes dans les données
nvar=ncol(data) # nombre de colonnes
ntest=ceiling(npop*test.ratio) # taille de l’échantillon test
testi=sample(1:npop,ntest) # indices de l’échantillon test
appri=setdiff(1:npop,testi) # indices complémentaires de l’échant. d’apprentiss
datapq=data[appri,] # construction de l’échantillon d’apprentissage
datatq=data[testi,] # construction de l’échantillon test
summary(datapq)
summary(datatq)


############################################################################################
#Echantillon apprentissage
############################################################################################

modgam<- gam(data=datapq, ozon~s(temp)+s(vent)+s(mozon))
summary(modgam)
Ozonepred <- predict(modgam)


par(mfrow = c(2,2))
plot(modgam,scale = 0,select=1, ylab="",ylim=c(-30,120), xlab="", main="Effet de la temperature", col="red")
plot(modgam,scale = 0,select=2, ylab="",ylim=c(-20,30), xlab="", main="Effet de la vitesse du vent", col="red")
plot(modgam,scale = 0,select=3, ylab="",ylim=c(-40,80), xlab="", main="Effet de la concentration d'ozone", col="red")


par(mfrow = c(1,1))
plot(datapq$ozon,Ozonepred,main="Nuage de points observé/estimé", xlab="Taux d'ozone observé",ylab="Taux d'ozone predit",pch=1,cex=0.8,cex.lab=1.5,font=2,font.lab=2,xlim=c(0,300),ylim=c(0,250))
abline(0,1,lwd=2,col=2)

par(mfrow = c(1,2))
residu1=Ozonepred-datapq$ozon
plot(residu1, main="Graphe des résidus", ylab="Résidus")
abline(h=0, lwd=2, col=2)

plot(datapq$ozon, residuals(modgam),xlab="Concentrations observés", ylab="Résidus", main="Biais des résidus")
abline(0,0,lwd=2,col=2)
abline(v=150, lwd = 2,col="red")

hist(residu1, col="red", main="Histogramme des résidus", ylab="Résidus")
shapiro.test(residu1)

Perfopm10(datapq$ozon, Ozonepred)
sd(residu1)

############################################################################################
#échantillon test
############################################################################################
Ozonepred2<-predict(modgam, newdata=datatq)
par(mfrow = c(1,2))
plot(datatq[,1],Ozonepred2, main="Nuage de points observés-prévus", ylab="Prévus", xlab="Observés")
abline(0,1,lwd=2,col=2)
res=Ozonepred2-datatq$ozon
plot(res, main="Graphe des résidus", ylab="Résidus")
abline(h=0, lwd=2, col=2)
hist(res, col="red", main="Histogramme des résidus", ylab="Résidus")
shapiro.test(res)
sd(res)/sqrt(243)

source("/Users/jean-eudes/Desktop/TP6/Moi/Perfo.R")
source("/Users/jean-eudes/Desktop/TP6/Moi/TabDep.R")

Perfopm10(datatq$ozon, Ozonepred2)
TabDep(datatq$ozon, Ozonepred2, 130,180,130)



####################################################################################
#Etude avec regression linéaire
####################################################################################

reslm <- lm(ozon~temp + vent + mozon,datapq)
summary(reslm)
Ozonepredlin <- predict(reslm)
par(mfrow = c(1,1))
plot(datapq[,1],Ozonepredlin, main="Nuage de points observés-estimés", ylab="Estimés", xlab="Observés")
abline(0,1,lwd=2,col=2)

reslin=Ozonepredlin-datapq$ozon
par(mfrow = c(1,2))
plot(reslin, main="Graphe des résidus", ylab="Résidus")
abline(h=0, lwd=2, col=2)
plot(datapq[,1], residuals(reslm), xlab="Concentrations observés", ylab="Résidus", main="Biais des résidus")
abline(0,0,lwd=2,col=2)
abline(v=150, lwd = 2,col="red")
Perfopm10(datapq$ozon, Ozonepredlin)
shapiro.test(residuals(reslm))

par(mfrow = c(1,2))
Ozonepred2lin <- predict(reslm, newdata=datatq)
plot(datatq[,1],Ozonepred2lin, main="Nuage de points observés-prévus", ylab="Prévus", xlab="Observés", cex.main=1)
abline(0,1,lwd=2,col=2)
reslin2=Ozonepred2lin-datatq$ozon
plot(reslin2, main="Graphe des résidus", ylab="Résidus",cex.main=1)
abline(h=0, lwd=2, col=2)

Perfopm10(datatq$ozon, Ozonepred2lin)
TabDep(datatq$ozon, Ozonepred2lin, 130,180,130)


sd(residuals(reslm))