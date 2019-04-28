#######################################
# Analyse de 2 variables quantitatives #
#     --> Age et Crawling_Weeks        #
#######################################

# Initialisation
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides Ã  la fin
View(dataSet)

#Analyse
Age <- as.numeric(unlist(dataSet["Age"]))
Crawling_Weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Age,Crawling_Weeks))
plot(Age,Crawling_Weeks,type = "p",main="corrélation entre l'âge et le temps de rampage",xlab="Age (jours)",ylab="Temps de rampage (semaines)",sub = cr)
# le coefficient de corrélation est très faible, pas de corrélation linéaire
# de plus sur le nuage de points on ne repère graphiquement aucun lien



##------- on créé des groupes pour chaque catégorie d'enfants
# groupe non Crawler et Crawler
NonCrawl <- subset(dataSet, Crawling_Group == "0") 
Crawler <- subset(dataSet, Crawling_Group == "1")

##------- on refait l'étude précédente chez les crawlers pour la phase de familiarisation
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
Crawling_Weeks <- as.numeric(unlist(Crawler["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Crawler_Predrate_Fam_FullOccl,Crawling_Weeks))
plot(Crawler_Predrate_Fam_FullOccl,Crawling_Weeks,type = "p",main="corrélation entre la prédiction et le temps de rampage",xlab="prédiction pourcentage",ylab="Temps de rampage (semaines)",sub = cr)
# coefficient de corrélation de 0.511 ce qui est pas fou, et graphiquement pas de changements énormes
# donc le temps de crawl ne semble pas vraiment influencer sur la prédiction

##------- on refait l'étude précédente entre pour la phase de tests
Crawler_Predrate_Test_FullOccl <- as.numeric(unlist(Crawler["Predrate_Test_FullOccl"]))
Crawler_Crawling_Weeks <- as.numeric(unlist(Crawler["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Crawler_Predrate_Test_FullOccl,Crawler_Crawling_Weeks))
plot(Crawler_Predrate_Test_FullOccl,Crawler_Crawling_Weeks,type = "p",main="corrélation entre la prédiction et le temps de rampage",xlab="prédiction pourcentage",ylab="Temps de rampage (semaines)",sub = cr)




print("Pour les enfants qui ne rampent pas")
NonCrawl_Predrate_Fam_FullOccl <- as.numeric(unlist(NonCrawl["Predrate_Fam_FullOccl"]))
summary(NonCrawl_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(NonCrawl_Predrate_Fam_FullOccl)))

print("Pour les enfants qui rampent")
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
summary(Crawler_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(Crawler_Predrate_Fam_FullOccl)))

# On observe que les crawlers ont en moyenne une meilleure prévisions que les non crawlers
