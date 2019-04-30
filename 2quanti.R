#######################################
# Analyse de 2 variables quantitatives #
#     --> Age et Crawling_Weeks        #
#######################################

# Initialisation
install.packages('haven')
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

#Analyse
Age <- as.numeric(unlist(dataSet["Age"]))
Crawling_Weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Age,Crawling_Weeks))
plot(Age,Crawling_Weeks,type = "p",main="corr?lation entre l'?ge et le temps de rampage",xlab="Age (jours)",ylab="Temps de rampage (semaines)",sub = cr)
# le coefficient de corr?lation est tr?s faible, pas de corr?lation lin?aire
# de plus sur le nuage de points on ne rep?re graphiquement aucun lien



##------- on cr?? des groupes pour chaque cat?gorie d'enfants
# groupe non Crawler et Crawler
NonCrawl <- subset(dataSet, Crawling_Group == "0") 
Crawler <- subset(dataSet, Crawling_Group == "1")

##------- on refait l'?tude pr?c?dente chez les crawlers pour la phase de familiarisation
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
Crawling_Weeks <- as.numeric(unlist(Crawler["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Crawler_Predrate_Fam_FullOccl,Crawling_Weeks))
plot(Crawler_Predrate_Fam_FullOccl,Crawling_Weeks,type = "p",main="corr?lation entre la pr?diction et le temps de rampage",xlab="pr?diction pourcentage",ylab="Temps de rampage (semaines)",sub = cr)
# coefficient de corr?lation de 0.511 ce qui est pas fou, et graphiquement pas de changements ?normes
# donc le temps de crawl ne semble pas vraiment influencer sur la pr?diction

##------- on refait l'?tude pr?c?dente entre pour la phase de tests
Crawler_Predrate_Test_FullOccl <- as.numeric(unlist(Crawler["Predrate_Test_FullOccl"]))
Crawler_Crawling_Weeks <- as.numeric(unlist(Crawler["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Crawler_Predrate_Test_FullOccl,Crawler_Crawling_Weeks))
plot(Crawler_Predrate_Test_FullOccl,Crawler_Crawling_Weeks,type = "p",main="corr?lation entre la pr?diction et le temps de rampage",xlab="pr?diction pourcentage",ylab="Temps de rampage (semaines)",sub = cr)




print("Pour les enfants qui ne rampent pas")
NonCrawl_Predrate_Fam_FullOccl <- as.numeric(unlist(NonCrawl["Predrate_Fam_FullOccl"]))
summary(NonCrawl_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(NonCrawl_Predrate_Fam_FullOccl)))

print("Pour les enfants qui rampent")
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
summary(Crawler_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(Crawler_Predrate_Fam_FullOccl)))

t.test(NonCrawl_Predrate_Fam_FullOccl, alternative="less",mu=mean(Crawler_Predrate_Fam_FullOccl))
# On observe que les crawlers ont en moyenne une meilleure pr?visions que les non crawlers confirmé par le test de Student
