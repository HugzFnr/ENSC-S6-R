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
#Corrélation assez haute des axes, peu de corrélation predrate-crawling et age-sexe
#ainsi qu'une forte corrélation interne crawling et predrate

#Sorties numériques
res$ind
round(res$quanti$cos2,digit=3)
res$quanti$contrib.pct

#On va se baser dans un premier temps sur la variable Condition, on choisit alors d'étudier le plan 2-4
plot(res,axes=c(2,4),choice="ind")
plot(res,axes=c(2,4),choice="cor")
plot(res,axes=c(2,4),choice="sqload")
#COrrélation basses des axes, peu de corrélation conditon-crawling

#de même pour Predrate_Fam_FullOccl, on choisit alors d'étudier le plan 1-5
plot(res,axes=c(1,5),choice="ind")
plot(res,axes=c(1,5),choice="cor")
plot(res,axes=c(1,5),choice="sqload")
#Corrélation moyenne des axes, peu d'infos

#de même pour Crawling_Group, on choisit alors d'étudier le plan 1-4
plot(res,axes=c(1,4),choice="ind")
plot(res,axes=c(1,4),choice="cor")
plot(res,axes=c(1,4),choice="sqload")
#Corrélation moyenne des axes, peu d'infos

#de même pour Sex, on choisit alors d'étudier le plan 3-5
plot(res,axes=c(3,5),choice="ind")
plot(res,axes=c(3,5),choice="cor")
plot(res,axes=c(3,5),choice="sqload")
#Corrélation moyenne des axes, peu de correlation age-sexe et sexe-Prop_incl_Trials_Fam

#Hypothèse : garder les axes 1-2-4 qui sont le plus représentatifs de nos variables