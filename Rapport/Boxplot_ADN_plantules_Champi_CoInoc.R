#Boxplot quantité ADN Champignons filamenteux plantules CoInoc
#Importation données
library(readxl)
adn_champi <- read_excel("ADN_Champi_CoInoc.xlsx", 
                         na = "NA")

#Mise en forme des données
adn_champi <- as.data.frame(adn_champi)
adn_champi$Champignon <- as.factor(adn_champi$Champignon)

#Séparation des données en sousgroupes selon le type de champignon
clado <- adn_champi[which(adn_champi$Champignon== "Clado"),]
aalt <- adn_champi[which(adn_champi$Champignon=="Aalt"),]
abra <- adn_champi[which(adn_champi$Champignon=="Abra43"),]

#Libraries
library(readxl)
library(ggplot2) 
library(agricolae)
library(pgirmess)
library(dunn.test)
library(multcomp)
library(dplyr)
library(ggbreak)
library(gridExtra)

#Grapiques
gclado<- ggplot(clado, aes(x=Condition,y=`Quantité d'ADN`, fill= Condition )) + theme(legend.position = "none") +
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("BFY Avec Abra", "BFY Sans Abra", "F Avec Abra", "F Sans Abra"), values = c("olivedrab3","darkolivegreen2","#00BFC4","cyan2"))

gaalt<- ggplot(aalt, aes(x=Condition,y=`Quantité d'ADN`, fill= Condition )) + theme(legend.position = "none") +
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("BFY Avec Abra", "BFY Sans Abra", "F Avec Abra", "F Sans Abra"), values = c("olivedrab3","darkolivegreen2","#00BFC4","cyan2"))

gabra <- ggplot(abra, aes(x=Condition,y=`Quantité d'ADN`, fill= Condition )) + theme(legend.position = "none") +
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("BFY Avec Abra", "BFY Sans Abra", "F Avec Abra", "F Sans Abra"), values = c("olivedrab3","darkolivegreen2","#00BFC4","cyan2"))

grid.arrange(gclado,gaalt,gabra, ncol = 3)

#Lettres significativités
##Pour Clado
#Vérification de la normalité des données
shapiro.test(clado$`Quantité d'ADN`)# --> p-value: 0.000599 < 0.05 --> pas normal --> faire test non paramétrique

#Test
kruskal.test(clado$`Quantité d'ADN`~clado$Condition ) #--> p-value: 0.07028 > 0.05 --> pas effet des CoInocs

#Lettres
kruskalmc(clado$`Quantité d'ADN`~clado$Condition, p.adjust.methods= "none")
mod_clado=kruskal(clado$`Quantité d'ADN`, clado$Condition)
mod_clado

##Pour Aalt
shapiro.test(aalt$`Quantité d'ADN`)# p-value: 0.7842 > 0.05 --> normal 
bartlett.test(aalt$`Quantité d'ADN` ~aalt$Condition) #p-value:  0.6925 > 0.05 --> pas différence des variances --> faire test paramétrique 

#Test
anova_aalt <- aov(`Quantité d'ADN`~Condition, data=aalt)
anova_aalt
summary(anova_aalt)
tukey_aalt <- HSD.test(anova_aalt, "Condition", group = TRUE )
tukey_aalt
#Lettres
lettres_aalt <- tukey$groups
lettres_aalt

##Pour Abra43
shapiro.test(abra$`Quantité d'ADN`) #p-value = 0.337 > 0.05 --> normal
var.test(abra$`Quantité d'ADN`~abra$Condition) #p-value = 0.201 > 0.05 --> pas différence des variances --> faire test paramétrique

#Test
anova_abra <- aov(`Quantité d'ADN`~Condition, data=abra)
anova_abra
summary(anova_abra)
tukey_abra <- HSD.test(anova_abra, "Condition", group = TRUE )
tukey_abra
#Lettres
lettres_abra <- tukey_abra$groups
lettres_abra
