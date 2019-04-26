#######################################
# Analyse de 2 variables qualitatives #
#     --> Sex et Crawling_group      #
#######################################

# Initialisation
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

sex <- as.numeric(unlist(dataSet["Sex"]))
group<- as.numeric(unlist(dataSet["Crawling_Group"]))

#Analyse
#on calcule le khi-deux de contingence
contigence<- table(sex,group)

nbValsGroup<-length(contigence[1,])
nbValsSex<-length(contigence[,1]) 
#on récupère le nombre de valeurs distinctes grâce à la table

#ici I==J
bornemax<-(length(sex)*(nbValsSex-1))
# 0 <= Khi² <= 33

res<-chisq.test(contigence) #test du khi-deux de contigence
res
#res$expected permet d'afficher la table de contigence standard pour vérifier

#le test calcule Khi² = 0,29219
#à la calculette j'ai 0,7926 , jsp pourquoi

#dans les deux cas c'est très bas par rapport à 33
#on peut conclure à une indépendance entre le sexe et la capacité de ramper

