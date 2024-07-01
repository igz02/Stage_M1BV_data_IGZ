#Boxplot Quantité ADN Abr43 plantules isolés CoInoc
setwd("C:/Users/Usuario/Documents/Stage M1/Data Figures") # à modifier selon l'emplacement des données

#Importation des données
library(readxl)

adn_abra <- read_excel("ADN_Abra_CoInoc.xlsx")

#Mise en forme des données 
adn_abra<- as.data.frame(adn_abra)

#Librairies 
library(readxl)
library(ggplot2) 
library(agricolae)
library(pgirmess)
library(dunn.test)
library(multcomp)
library(dplyr)

#Représentation graphique
ggplot(adn_abra, aes(x=Condition,y=`Quantité d'ADN`, fill= Condition)) + 
  geom_boxplot() + 
  scale_x_discrete(limits = c("Y","H2O", "F", "BF", "BFY", "B", "FY", "BY")) # modification de l'ordre du graphique pour l'oral pour qu'il match l'ordre du stackplot

#Test statistique

#Vérification de la normalité des données
shapiro.test(adn_abra$`Quantité d'ADN`) # --> p-value: 1.378e-05 --> faire test non paramétrique

#Test
kruskal.test(adn_abra$`Quantité d'ADN`~adn_abra$Condition )

#Lettres
kruskalmc(adn_abra$`Quantité d'ADN`~adn_abra$Condition, p.adjust.methods= "none")
mod_abra=kruskal(adn_abra$`Quantité d'ADN`, adn_abra$Condition)
mod_abra

#Les lettres sont placées manuellement sur le graphique