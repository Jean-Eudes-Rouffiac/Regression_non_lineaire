# ----------------------------------------------------
#TP1 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------

Nomfile =  "http://lmi2.insa-rouen.fr/~bportier/Data/chenille.txt"
chenille <-read.table(file=Nomfile, header=T,dec=".")
attach(chenille)
plot(chenille)
pairs(chenille,gap=0.05,cex=0.8,col="purple")
cor(chenille)

#Oblige de passer par lm pour calculer le VIF mais on introduit pas encore le modele
reslm <- lm(Y~X1+ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 ,chenille)
library(car)
vif(reslm)

#Coefficients du modele
model<-lm(Y~X1+ X2 + X3 + X4 + X5 + X7 + X8 + X9 + X10 ,chenille)
model$coefficients


summary(lm(Y~X1 + X2 + X3 + X4 + X5 + X7 + X8 + X9 + X10))

TitreRes = "Graphe des résidus"
plot(residuals(model), xlab="Index", ylab="Résidus",main=
       TitreRes , cex=0.6, cex.lab=1, cex.main = 1,cex.axis=1)
abline(0,0, col=2, lwd=2)

Titre1 = "Graphe des résidus studentisés"
plot(rstudent(model),xlab="Index", ylab="Résidus studentisés",main=
       Titre1 , cex=0.6, cex.lab=1, cex.main = 1,cex.axis=1, ylim=c(-2.8,3))
p=9
seuil <- qt(0.975,n-p-2)
abline(h=c(-seuil , 0, seuil),col=2)

Titre = "Nuage de points (Observés/Prévus)"
plot(Y,predict(model), xlab="Nombre moyen de nid observé", ylab="Nombre moyen de nid estimé",main= Titre , cex=0.8, cex.lab=1, cex.main =
       1,cex.axis=1, ylim=c(-0.8,2))
abline(0,1, col=2, lwd=2)

shapiro.test(model$residuals)
hist(residuals(model),col="#CC00CC",xlab="Résidus",ylab="Fréquences",main="Histogramme des résidus",tck=0.01, freq=FALSE)
durbinWatsonTest(reslm) 

#Backward regression
# --------------------#
summary(lm(Y~X1+ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10))
summary(lm(Y~X1+ X2 + X3 + X4 + X5 + X6 + X8 + X9 + X10))
summary(lm(Y~X1+ X2 + X3 + X4 + X5 + X6 + X9 + X10))
summary(lm(Y~X1+ X2 + X3 + X4 + X5 + X9 + X10))
summary(lm(Y~X1+ X2 + X4 + X5 + X9 + X10))
summary(lm(Y~X1+ X2 + X4 + X5 + X9))
summary(lm(Y~X1+ X2 + X4 + X5))

#Coefficients du nouveau modele
model<-lm(Y~X1+ X2 + X4 + X5 ,chenille)
model$coefficients
summary(lm(Y~X1 + X2 + X4 + X5))


#Forward regression
# --------------------#
summary(lm(Y ~X9 + X1 + X2 + X3))
summary(lm(Y ~X9 + X1 + X2 + X4))
summary(lm(Y ~X9 + X1 + X2 + X5))
summary(lm(Y ~X9 + X1 + X2 + X6))
summary(lm(Y ~X9 + X1 + X2 + X7))
summary(lm(Y ~X9 + X1 + X2 + X8))


#Modèle simplifié
# --------------------#

modelfinal<-lm(Y~X1 + X2 + X4 + X5 ,chenille)
modelfinal$coefficients
summary(lm(Y~X1 + X2 + X4 + X5))

TitreRes = "Graphe des résidus"
plot(residuals(modelfinal), xlab="Index", ylab="Résidus",main=
       TitreRes , cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5)
abline(0,0, col=2, lwd=2)

Titre = "Nuage de points (Observés/Prévus)"
plot(Y,predict(modelfinal), xlab="Nombre moyen de nid observé", ylab="Nombre moyen de nid estimé",main= Titre , cex=1.5, cex.lab=1.6, cex.main =
       1.7,cex.axis=1.5)
abline(0,1, col=2, lwd=2)

plot(rstudent(modelfinal))
p=4
seuil <- qt(0.975,n-p-2)
abline(h=c(-seuil , 0, seuil))
lines(loewess(rstudent(modelfinal)))
plot(cooks.distance(modelfinal),type="h")
