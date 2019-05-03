 ######################################
#      Partie stat inferentielle       #
#        Crawler et non Crawler        #
 ######################################

# est ce que les rampants ont de meilleures predictions que les non rampants?

# Initialisation
install.packages("haven")
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin

# on cree des groupes pour chaque categorie d'enfants non Crawler et Crawler
NonCrawl <- subset(dataSet, Crawling_Group == "0") 
Crawler <- subset(dataSet, Crawling_Group == "1")

# Etude pourles enfants qui ne rampent pas
NonCrawl_Predrate_Fam_FullOccl <- as.numeric(unlist(NonCrawl["Predrate_Fam_FullOccl"]))
summary(NonCrawl_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(NonCrawl_Predrate_Fam_FullOccl)))

# Etude pourles enfants qui rampent
Crawler_Predrate_Fam_FullOccl <- as.numeric(unlist(Crawler["Predrate_Fam_FullOccl"]))
summary(Crawler_Predrate_Fam_FullOccl)
print(paste("ecart-type = ",sd(Crawler_Predrate_Fam_FullOccl)))

shapiro.test(NonCrawl_Predrate_Fam_FullOccl)
# p-value > 0.05 donc on peut effectuer une 
# modélisation par une loi normale de Predrate_Fam_FullOccl 
shapiro.test(Crawler_Predrate_Fam_FullOccl)
# p-value > 0.05 donc on peut effectuer une 
# modélisation par une loi normale de Predrate_Fam_FullOccl 
# Ainsi on peut réaliser un test de Student d'égalité des moyennes Cependant tres petit echantillon et vraiment limite la p_value
t.test(NonCrawl_Predrate_Fam_FullOccl, alternative="less",mu=mean(Crawler_Predrate_Fam_FullOccl))
# On observe que les crawlers ont en moyenne une meilleure previsions que les non crawlers confirme par le test de Student
