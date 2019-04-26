#######################################
# Analyse de 2 variables qualitatives #
#     --> Sexe et Crawling_group      #
#######################################

# Initialisation
library(haven)
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

#Analyse
