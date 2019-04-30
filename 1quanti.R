#######################################
# Analyse d'une variable quantitative #
#     --> Crawling_Weeks              #
#######################################

# Initialisation
install.packages('haven')
library(haven)
rm(list=ls())
dataSet <- read_sav("Kubicek et al._JECP_DataSet.sav")
dataSet <- subset(dataSet, Age != "NA") #suppression des lignes vides à la fin
View(dataSet)

crawling_weeks <- as.numeric(unlist(dataSet["Crawling_Weeks"]))

# Analyse
summary(crawling_weeks) #général
sd(crawling_weeks) #dispersion
par(mfrow=c(1,2))
hist(crawling_weeks)
boxplot(crawling_weeks)
