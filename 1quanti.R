#######################################
# Analyse d'une variable quantitative #
#     --> Crawling_Weeks              #
#######################################

# Initialisation
library(haven)
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

crawling_weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))

# Analyse
summary(crawling_weeks) #général
sd(crawling_weeks) #dispersion
