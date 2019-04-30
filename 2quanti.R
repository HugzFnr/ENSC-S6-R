#######################################
# Analyse de 2 variables quantitatives #
#     --> Age et Crawling_Weeks        #
#######################################

# Initialisation
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

## on cree des groupes pour chaque categorie d'enfants
# groupe non Crawler et Crawler
NonCrawl <- subset(dataSet, Crawling_Group == "0") 
Crawler <- subset(dataSet, Crawling_Group == "1")

#Analyse Age et Crawling_Weeks
## ------
Age <- as.numeric(unlist(dataSet["Age"]))
Crawling_Weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Age,Crawling_Weeks))
plot(Age,Crawling_Weeks,type = "p",main="correlation entre l'age et le temps de rampage",xlab="Age (jours)",ylab="Temps de rampage (semaines)",sub = cr)
# le coefficient de correlation est tres faible 0.26139, pas de correlation lineaire
# de plus sur le nuage de points on ne repere graphiquement aucun lien, on reitere l'etude juste avec les
# crawlers

Crawler_Age <- as.numeric(unlist(Crawler["Age"]))
Crawler_Crawling_Weeks <- as.numeric(unlist(Crawler["Crawling_Weeks"]))
cr = paste("coefcorrelation = ", cor(Crawler_Age,Crawler_Crawling_Weeks))
plot(Crawler_Age,Crawler_Crawling_Weeks,type = "p",main="correlation entre l'age et le temps de rampage (rampants)",xlab="Age (jours)",ylab="Temps de rampage (semaines)",sub = cr)
# le coefficient de correlation est encore plus faible 0.12167, pas de correlation lineaire
# de plus sur le nuage de points on ne repere graphiquement aucun lien
## ------

# Analyse Predrate_Fam_FullOccl et Predrate_Fam_TarOccl
## ------

Predrate_Fam_FullOccl <- as.numeric(unlist(dataSet["Predrate_Fam_FullOccl"]))
Predrate_Fam_TarOccl <- as.numeric(unlist(dataSet["Predrate_Fam_TarOccl"]))
cr = paste("coefcorrelation = ", cor(Predrate_Fam_FullOccl,Predrate_Fam_TarOccl))
plot(Predrate_Fam_FullOccl,Predrate_Fam_TarOccl,type = "p",main="correlation entre Predrate_Fam_FullOccl et Predrate_Fam_TarOccl",xlab="Predrate_Fam_FullOccl",ylab="Predrate_Fam_TarOccl",sub = cr)
# le coefficient de correlation est tres élevé 0.918, forte correlation lineaire
lm(Predrate_Fam_TarOccl ~ Predrate_Fam_FullOccl)
#(Intercept)  Predrate_Fam_FullOccl  
#-0.745                  1.020
abline(-0.745 , 1.020, col = 'red')
## Conclusion, pour la phase de familiarisation il y a une corrélation linéaire quasi parfaite entre 
## ------


# Partie stat inferentielle, est ce que les rampants ont de meilleures predictions que les non rampants?
## ----
print("Pour les enfants qui ne rampent pas")
NonCrawl_Predrate_Fam_FullOccl <- as.numeric(unlist(NonCrawl["Predrate_Fam_FullOccl"]))
summary(NonCrawl_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(NonCrawl_Predrate_Fam_FullOccl)))

print("Pour les enfants qui rampent")
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
summary(Crawler_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(Crawler_Predrate_Fam_FullOccl)))

shapiro.test(NonCrawl_Predrate_Fam_FullOccl)
#p-value > 0.05 Donc on peut realiser un test de Student Cependant tres petit echantillon et vraiment limite la p-value
shapiro.test(Crawler_Predrate_Fam_FullOccl)
#p-value > 0.05 Donc on peut realiser un test de Student Cependant tres petit echantillon et vraiment limite la p_value
t.test(NonCrawl_Predrate_Fam_FullOccl, alternative="less",mu=mean(Crawler_Predrate_Fam_FullOccl))
# On observe que les crawlers ont en moyenne une meilleure previsions que les non crawlers confirme par le test de Student
