# ----------------------------------------------------
#TP8 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------


library(rpart)
library(randomForest)
source("/Users/jean-eudes/Desktop/TP8/Moi/Fig_obspm10.R")
source("/Users/jean-eudes/Desktop/TP8/Moi/Perfopm10.R")
source("/Users/jean-eudes/Desktop/TP8/Moi/TabDeppm10.R")
#data<-read.table("Data_HRI.txt", header=TRUE,sep=";")
data<-read.table("/Users/jean-eudes/Desktop/TP8/Moi/Data_HRI.txt", header=TRUE,sep=";")
attach(data)
#summary(data)


dataNA = na.omit(data) # Elimination des donnéesq manquantes
attach(dataNA)
set.seed(111) # initialisation du générateur
test.ratio=.2 # part de l’échantillon test
npop=nrow(dataNA) # nombre de lignes dans les données
nvar=ncol(dataNA) # nombre de colonnes
ntest=ceiling(npop*test.ratio) # taille de l’échantillon test
testi=sample(1:npop,ntest) # indices de l’échantillon test
appri=setdiff(1:npop,testi) # indices complémentaires de l’échant. d’apprentiss
datapq=dataNA[appri,] # construction de l’échantillon d’apprentissage
datatq=dataNA[testi,] # construction de l’échantillon test
#summary(datapq)
#summary(datatq)




###########################################################
###########################################################
##    Arbre max
###########################################################
###########################################################

modcart <- rpart(formula(dataNA),data = dataNA[appri,])
summary(modcart)
print(modcart)


plot(modcart, branch = 0.1, uniform = T)

text(modcart, digit = 4,col="red", cex=0.7)
title("Modélisation des PM10")




## Perf arbre max prediction app
pm10est = predict(modcart)
Perfopm10(dataNA$PM10[appri],pm10est)
TabDeppm10(dataNA$PM10[appri],pm10est,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon d'apprentissage",sep="\n")
Fig_obspm10(dataNA$PM10[appri],pm10est,Titre,"Essai")

## Perf arbre max prediction test
pm10est = predict(modcart, newdata = datatq)
Perfopm10(dataNA$PM10[testi],pm10est)
TabDeppm10(dataNA$PM10[testi],pm10est,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon test",sep="\n")
Fig_obspm10(dataNA$PM10[testi],pm10est,Titre,"Essai")



###########################################################
###########################################################
##    Arbre élagué
###########################################################
###########################################################

plotcp(modcart)
modcartpr <- prune(modcart, cp = 0.021) #CP à lire graphiquement
plot(modcartpr, branch = 0.3, uniform = T)
text(modcartpr, digit = 4,col=2)
title("Modélisation des PM10 cas de l'arbre élagué")



##Performance élagué prediction avec echantillon d'apprentissage :

pm10estEl = predict(modcartpr)
Perfopm10(dataNA$PM10[appri],pm10estEl)
TabDeppm10(dataNA$PM10[appri],pm10estEl,30,50,30)
Titre = paste("Station HRI - Arbre élagué","Echantillon d'apprentissage",sep="\n")
Fig_obspm10(dataNA$PM10[appri],pm10estEl,Titre,"Essai")


##Performance élagué prediction avec echantillon test :

pm10estEl = predict(modcartpr, newdata=datatq)
Perfopm10(dataNA$PM10[testi],pm10estEl)
TabDeppm10(dataNA$PM10[testi],pm10estEl,30,50,30)
Titre = paste("Station HRI - Arbre élagué","Echantillon test",sep="\n")
Fig_obspm10(dataNA$PM10[testi],pm10estEl,Titre,"Essai")



###########################################################
###########################################################
##    Arbre Touffu
###########################################################
###########################################################
modcartTouf <- rpart(formula(dataNA),data = dataNA[appri,],control=rpart.control(cp=0.003))
summary(modcartTouf)
print(modcartTouf)
plot(modcartTouf, branch = 0.3, uniform = T)
text(modcartTouf, digit = 2,col=2)
title("Modélisation des PM10")


#Elagage
modcartprTouf <- prune(modcartTouf, cp = 0.009) #CP à lire graphiquement
plot(modcartprTouf, branch = 0.3, uniform = T)
text(modcartprTouf, digit = 5)

##Performance touffu prediction avec echantillon apprentissage :
pm10estTouf = predict(modcartTouf)
Perfopm10(dataNA$PM10[appri],pm10estTouf)
TabDeppm10(dataNA$PM10[appri],pm10estTouf,30,50,30)
Titre = paste("Station HRI - Arbre touffu","Echantillon apprentissage",sep="\n")
Fig_obspm10(dataNA$PM10[appri],pm10estTouf,Titre,"Essai")

##Performance touffu prediction avec echantillon test :
pm10estTouf = predict(modcartTouf, newdata=datatq)
Perfopm10(dataNA$PM10[testi],pm10estTouf)
TabDeppm10(dataNA$PM10[testi],pm10estTouf,30,50,30)
Titre = paste("Station HRI - Arbre touffu","Echantillon test",sep="\n")
Fig_obspm10(dataNA$PM10[testi],pm10estTouf,Titre,"Essai")


###########################################################
###########################################################
##    Forets
###########################################################
###########################################################

modrf <- randomForest(formula(dataNA), data = dataNA,importance=T)
round(importance(modrf), 2)
Titre ="Station JUS - Importance des variables par les Forets Aleatoires"
op <- par(mfrow=c(1, 2))
varImpPlot(modrf, sort=TRUE, type=1, main="")
varImpPlot(modrf, sort=TRUE, type=2, main="")
par(op)


##Forets aléatoires
modcart <- randomForest(PM10 ~ SO2 + NO2 + GTlehavre + NO + T.max + DV.maxvv + VV.moy + PA.moy + HR.moy, data = dataNA[appri,],importance=TRUE)
impvar = c("SO2", "NO2", "NO" , "GTlehavre" , "T.max" , "DV.maxvv" , "VV.moy" , "PA.moy" , "HR.moy")
op <- par(mfrow=c(3, 3))
for (i in seq_along(impvar)) {
  partialPlot(modcart, dataNA[appri,], impvar[i], xlab=impvar[i],  main=paste("Effet de", impvar[i]),lwd=1.8, cex.lab=1.6,cex.main=1.6,cex.axis=1.2, col=2)
}
par(op)


# Preformance echantillon apprentissage
pm10estfor = predict(modcart)
Perfopm10(dataNA$PM10[appri],pm10estfor)
TabDeppm10(dataNA$PM10[appri],pm10estfor,30,50,30)
Titre = paste("Station HRI - Foret","Echantillon
              d'apprentissage",
              sep="\n")
Fig_obspm10(dataNA$PM10[appri],pm10estfor,Titre,"Essai")

# Preformance echantillon test
pm10estfor = predict(modcart, newdata=datatq)
Perfopm10(dataNA$PM10[testi],pm10estfor)
TabDeppm10(dataNA$PM10[testi],pm10estfor,30,50,30)
Titre = paste("Station HRI - Foret ","Echantillon
              test",
              sep="\n")
Fig_obspm10(dataNA$PM10[testi],pm10estfor,Titre,"Essai")





###########################################################
###########################################################
##    Comparaison modèle lm
###########################################################
###########################################################

modlm <- lm(PM10 ~ SO2 + NO2 + GTlehavre + NO + T.max + DV.maxvv + VV.moy + PA.moy + HR.moy ,data =datapq) #Après vif
vif(modlm)

## Perf modele lm data app
PM10estlm <- predict(modlm)
Perfopm10(dataNA$PM10[appri],PM10estlm)
TabDeppm10(dataNA$PM10[appri],PM10estlm,30,50,30)
Titre = paste("Station HRI - Modèle lm","Echantillon apprentissage",sep="\n")
Fig_obspm10(dataNA$PM10[appri],PM10estlm,Titre,"Essai")

## Perf modele lm data test
Pm10predlm = predict(modlm, newdata = datatq)
Perfopm10(dataNA$PM10[testi],Pm10predlm)
TabDeppm10(dataNA$PM10[testi],Pm10predlm,30,50,30)
Titre = paste("Station HRI - Modèle lm","Echantillon test",sep="\n")
Fig_obspm10(dataNA$PM10[testi],Pm10predlm,Titre,"Essai")

###########################################################
###########################################################
##    Comparaison modèle gam
###########################################################
###########################################################


library(gam)
modgam<- gam(data=datapq, PM10~ s(SO2) + s(NO2) + s(GTlehavre) + s(NO) + s(T.max) + s(DV.maxvv) + s(VV.moy) + s(PA.moy) + s(HR.moy))
summary(modgam)


## Perf modele gam data app
PM10estgam <- predict(modgam)
Perfopm10(dataNA$PM10[appri],PM10estgam)
TabDeppm10(dataNA$PM10[appri],PM10estgam,30,50,30)
Titre = paste("Station HRI - Modèle gam","Echantillon apprentissage",sep="\n")
Fig_obspm10(dataNA$PM10[appri],PM10estgam,Titre,"Essai")

## Perf modele gam data test
Pm10predgam = predict(modgam, newdata = datatq)
Perfopm10(dataNA$PM10[testi],Pm10predgam)
TabDeppm10(dataNA$PM10[testi],Pm10predgam,30,50,30)
Titre = paste("Station HRI - Modèle gam","Echantillon test",sep="\n")
Fig_obspm10(dataNA$PM10[testi],Pm10predgam,Titre,"Essai")

