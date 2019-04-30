rm(list=ls())
library(haven)
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

dataFrame <- as.data.frame(dataSet)
require(PCAmixdata)
res <- PCAmix(dataFrame)

#Choix param ? retenir
round(res$eig,digit=2)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

#Graphe des individus sur le plan factoriel 1-2
plot(res,axes=c(1,2),choice="ind")
plot(res,axes=c(1,2),choice="cor")
plot(res,axes=c(1,2),choice="sqload")

#Sorties numériques
res$ind
round(res$ind$cos2,digit=3)
res$quanti$contrib.pct