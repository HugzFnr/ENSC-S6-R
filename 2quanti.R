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

#Analyse
