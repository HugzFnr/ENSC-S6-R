#######################################
# Analyse d'une variable quantitative #
#     --> Crawling_Weeks              #
#######################################

# Initialisation
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides ? la fin
View(dataSet)

crawling_weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))

# Analyse avec tout le dataset
summary(crawling_weeks) #général
sd(crawling_weeks) #dispersion
par(mfrow=c(1,2))
hist(crawling_weeks, main="Histogramme : répartition de Crawling_Weeks")
boxplot(crawling_weeks, main="Boxplot : répartition de Crawling_Weeks")

#Analyse avec seulement les enfants qui savent ramper
#datasetRampants <- dataSet
datasetRampants <- subset(dataSet, Crawling_Group == 1)
View(datasetRampants)
