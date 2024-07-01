#Boxplot Ct pool graines SYNCOM----

#Sélection du working directory
setwd("C:/Users/Usuario/Documents/Stage M1/Data Figures")# à changer selon l'emplacement des données

#Importations des données
library(readr)

pgraines <- read.csv("Pool_Graines_SynCom.csv", header = TRUE, sep = ";", dec = ",", na.strings = "NA", stringsAsFactors = FALSE)

#Libraries
library(ggplot2)
library(reshape2)
library(ggbreak)

#Boxplot avec points dispersés
ggplot(pgraines, aes(x=Condition,y=Value, fill= Condition)) + geom_boxplot() + geom_jitter(shape=16,position = position_jitter(0.2))

#Boxplot avec des points centrés
ggplot(pgraines, aes(x=Condition,y=Value, fill= Condition)) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) #avec points centrés
