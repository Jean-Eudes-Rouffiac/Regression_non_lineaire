# ----------------------------------------------------
#TP7 - JE ROUFFIAC - REGRESSION NON LINEAIRE 2018/2019
# ----------------------------------------------------


nomfile = "http://lmi2.insa-rouen.fr/~bportier/Data/dataTPUSI.csv"
data <- read.csv2(nomfile, header = TRUE,sep=";")
attach(data)

#Histogrammes :

par(mfrow = c(2,2))
hist(AGE,col="cyan", main="Histogramme répartition de l'age")
hist(FC,col="cyan", main="Histogramme répartition de la fréquence cardiaque")
hist(TAS,col="cyan", main="Histogramme répartition de la tension")

par(mfrow = c(1,1))
plot(AGE, STA,pch=1,ylab="STA", xlab="AGE", main="Nuage de points STA en fonction de AGE")
plot(FC, STA,pch=1,ylab="STA", xlab="FC", main="Nuage de points STA en fonction de FC")
plot(TAS, STA,pch=1,ylab="STA", xlab="TAS", main="Nuage de points STA en fonction de TAS")


chi<-matrix(nrow = 21, ncol = 2)
for (i in 1:21) {
  chi[i,1]<-colnames(data)[i]
  chi[i,2]<-chisq.test(table(data[,i],STA))$p.value
}
#On garde : SER - IRC - INF - MCE - TYP - CRE - CS

#FC sert à rien

STAreglog <- glm(STA ~ AGE + TAS + SER + IRC + INF + MCE + TYP + CRE + CS, family = binomial, trace=TRUE)
summary(STAreglog)
anova(STAreglog)


STAreglogpred = ifelse(predict(STAreglog,type="response")>0.5,1,0)
table(STA,STAreglogpred)

#Etude descendante :
summary(glm(STA ~ AGE + TAS + SER + IRC + INF + MCE + TYP + CRE + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + IRC + INF + MCE + TYP + CRE + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + IRC + MCE + TYP + CRE + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + IRC + MCE + TYP + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + IRC + TYP + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + TYP + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TYP + CS, family = binomial, trace=TRUE))

ModRed <- glm(STA ~ AGE + TYP + CS, family = binomial, trace=TRUE)
summary(ModRed)

Modredpred = ifelse(predict(ModRed,type="response")>0.5,1,0)
table(STA,Modredpred)



#Études des résidus :
resid.pea <- residuals.glm(ModRed,type="pearson")
resid.dev <- residuals.glm(ModRed,type="deviance")
par(mfrow = c(1,1))
plot(resid.pea, font.lab=2,pch=1,ylab="Résidus", main="Graphe des résidus de Pearson")


plot(resid.dev, font.lab=2,pch=1,ylab="Résidus", main="Graphe des résidus de déviance expliquée")


CS <- as.factor(CS)
ModRed2 <- glm(STA ~ AGE + TYP + CS, family = binomial, trace=TRUE)
summary(ModRed2)
res2 <- residuals.glm(ModRed2,type="pearson")
plot(res2)

ModRed2pred = ifelse(predict(ModRed2,type="response")>0.5,1,0)
table(STA,ModRed2pred)


##Modèle proposé par le médecin
#Analyse descendante :
ModMedComp <- glm(STA ~ AGE + CAN + IRC + INF + TAS + CS, family = binomial, trace=TRUE)
summary(ModMedComp)

ModMedComppred = ifelse(predict(ModMedComp,type="response")>0.5,1,0)
table(STA,ModMedComppred)

summary(glm(STA ~ AGE + CAN + IRC + INF + TAS + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + IRC + INF + TAS + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + IRC + TAS + CS, family = binomial, trace=TRUE))
summary(glm(STA ~ AGE + TAS + CS, family = binomial, trace=TRUE))

ModMed<-glm(STA ~ AGE + TAS + CS, family = binomial, trace=TRUE)
summary(ModMed)
res3<-ModMed$residuals
plot(res3)

ModMedpred = ifelse(predict(ModMed,type="response")>0.5,1,0)
table(STA,ModMedpred)


#ajout d'un modele comme le prof avait dit en cours
ModMed3<-glm(STA ~ AGE + TYP + CS + ID + CAN + PH + PCO, family = binomial, trace=TRUE)
summary(ModMed3)
ModMed3pred = ifelse(predict(ModMed3,type="response")>0.5,1,0)
table(STA,ModMed3pred)


#Étude avec les courbes ROC :
library("ROCR")


par(mfrow = c(2,2))
pred=predict(STAreglog)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle complet statisticien", cex.main=0.9)

pred=predict(ModRed)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle réduit statisticien", cex.main=0.9)


pred=predict(ModMedComp)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle complet médecin ", cex.main=0.9)

pred=predict(ModMed)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle réduit médecin", cex.main=0.9)




#comparaison des deux modèles qui semblent les meilleurs
par(mfrow = c(1,1))
pred1=predict(ModMed3)
p1=prediction(pred1,STA)
perf1=performance(p1,"tpr", "fpr")
pred2=predict(ModRed2)
p2=prediction(pred2,STA)
perf2=performance(p2,"tpr", "fpr")
plot(perf1,colorize=TRUE, main="Comparaison courbe ROC modèle Total réduit et modèle réduit avec CS as factor",cex.main=0.8)
plot(perf2,add=TRUE,col=2)


#comparaison de tous les modèles
par(mfrow = c(1,1))
pred1=predict(STAreglog)
p1=prediction(pred1,STA)
perf1=performance(p1,"tpr", "fpr")
pred2=predict(ModRed)
p2=prediction(pred2,STA)
perf2=performance(p2,"tpr", "fpr")
pred3=predict(ModMedComp)
p3=prediction(pred3,STA)
perf3=performance(p3,"tpr", "fpr")
pred4=predict(ModMed)
p4=prediction(pred4,STA)
perf4=performance(p4,"tpr", "fpr")
pred5=predict(ModMed3)
p5=prediction(pred5,STA)
perf5=performance(p5,"tpr", "fpr")
pred6=predict(ModRed2)
p6=prediction(pred6,STA)
perf6=performance(p6,"tpr", "fpr")
plot(perf1,col=2, main="comparaison")
plot(perf2,add=TRUE,col=1)
plot(perf3,add=TRUE,col=3)
plot(perf4,add=TRUE,col=4)
plot(perf5,add=TRUE,col=5)
plot(perf6,add=TRUE,col=6)


pred=predict(ModRed3)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle réduit")

pred=predict(ModRed2)
p=prediction(pred,STA)
perf=performance(p,"tpr", "fpr")
plot(perf,colorize = TRUE, main="Courbe ROC Modèle réduit avec CS as factor")
