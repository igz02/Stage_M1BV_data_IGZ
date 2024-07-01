
setwd("C:/Users/Usuario/Documents/Stage M1/Data Figures")#A changer selon l'emplacement des données

library(readxl)
data <- read_excel("Quantite ADN VEGLAB Stage.xlsx", 
                   na = "NA")

#Mise en forme des données
data <- as.data.frame(data)
data$Condition <- as.factor(data$Condition)
data$Type <- as.factor(data$Type)

#Séparation en sous-groupes 
b<- data[which(data$Condition== "B"),]
bf <- data[which(data$Condition== "BF"),]
bfy <- data[which(data$Condition== "BFY"),]
by <- data[which(data$Condition== "BY"),]
f <- data[which(data$Condition== "F"),]
fy <- data[which(data$Condition== "FY"),]
h2o <- data[which(data$Condition== "H2O"),]
y <- data[which(data$Condition== "Y"),]
veglab <- data[which(data$Type=="SynCom"),]

#Graphiques----

library(ggplot2)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(ggbreak)
library(gridExtra)
library(agricolae)
library(pgirmess)

##Pour quantité ADN présent pour SynCom
ggplot(veglab, aes(x=Condition, y=`Quantité d'ADN`, fill=Condition)) + 
  geom_boxplot() + 
  scale_fill_manual(breaks = c("B", "BF", "BFY", "BY", "F", "FY", "H2O", "Y"), 
                    values = c("lightcoral", "darkgoldenrod2", "darkolivegreen2", "seagreen3", 
                               "cyan2", "deepskyblue2", "darkorchid1", "plum2")) +
  scale_x_discrete(limits = c("H2O","Y","FY", "BY", "BFY", "B", "BF", "F"))#changement de l'ordre de positionnement des boxplots selon l'ordre présenter sur le stackplot


#Pour lettres
shapiro.test(veglab$`Quantité d'ADN`) #p-value = 3.897e-06 --> non paramétrique 

kruskal.test(veglab$`Quantité d'ADN`~veglab$Condition ) 
kruskalmc(veglab$`Quantité d'ADN`~veglab$Condition, p.adjust.methods= "none")
mod_veglab=kruskal(veglab$`Quantité d'ADN`, veglab$Condition)
mod_veglab

##Pour B----
B<- ggplot(b, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("coral1","lightcoral")) + 
  labs(title="B") + 
  theme(legend.position = "none")
##Pour BF
BF<- ggplot(bf, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("darkgoldenrod","darkgoldenrod2")) + 
  labs(title="BF") +
  theme(legend.position = "none")
##Pour BFY
BFY <- ggplot(bfy, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("olivedrab3","darkolivegreen2")) + 
  labs(title="BFY") +
  theme(legend.position = "none")
##Pour BY
BY <- ggplot(by, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("#00BA38","seagreen3")) + 
  labs(title="BY") + theme(legend.position = "none")
##Pour F
Fu <- ggplot(f, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("#00BFC4","cyan2")) + 
  labs(title="F") + theme(legend.position = "none")
## Pour FY
FY <- ggplot(fy, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("dodgerblue1","deepskyblue2")) + 
  labs(title="FY") + theme(legend.position = "none")
## Pour H2O
H2O <- ggplot(h2o, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("mediumorchid1", "darkorchid1")) + 
  labs(title="H2O") + theme(legend.position = "none")
## Pour Y 
Y <- ggplot(y, aes(x=Type,y=`Quantité d'ADN`, fill= Type )) + 
  geom_boxplot()  + 
  scale_fill_manual(breaks = c("CoInoc", "SynCom"), values = c("#F564E3", "plum2")) + 
  labs(title="Y") + theme(legend.position = "none")

#Stack graphique 
grid.arrange(B,BF,BFY,BY,Fu,FY,H2O,Y, ncol = 4)

#Lettres----
#Test de Normalité
shapiro.test(b$`Quantité d'ADN`) # p-value = 0.1445
shapiro.test(bf$`Quantité d'ADN`) # p-value = 0.01533 --> non paramétrique
shapiro.test(bfy$`Quantité d'ADN`) # p-value = 0.2928, par contre peu de données
shapiro.test(by$`Quantité d'ADN`) # p-value = 0.0501
shapiro.test(f$`Quantité d'ADN`) # p-value = 0.01686 --> non paramétrique
shapiro.test(fy$`Quantité d'ADN`) #p-value = 0.01343 --> non paramétrique
shapiro.test(h2o$`Quantité d'ADN`) #p-value = 0.01556 --> non paramétrique
shapiro.test(y$`Quantité d'ADN`) #p-value = 0.07716 

#Test Homogéinité des Variances
var.test(b$`Quantité d'ADN`~b$Type) # p-value = 0.007519 --> non paramétrique
var.test(by$`Quantité d'ADN`~by$Type) # p-value = 0.004381 --> non paramétrique
var.test(y$`Quantité d'ADN`~y$Type) # p-value = 0.004632 --> non paramétrique


#Test non paramétrique
##B
kruskal.test(b$`Quantité d'ADN`~b$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(b$`Quantité d'ADN`~b$Type, p.adjust.methods= "none")
mod_b=kruskal(b$`Quantité d'ADN`, b$Type)
mod_b
## BY
kruskal.test(by$`Quantité d'ADN`~by$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(by$`Quantité d'ADN`~by$Type, p.adjust.methods= "none")
mod_by=kruskal(by$`Quantité d'ADN`, by$Type)
mod_by
## BF
kruskal.test(bf$`Quantité d'ADN`~bf$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(bf$`Quantité d'ADN`~bf$Type, p.adjust.methods= "none")
mod_bf=kruskal(bf$`Quantité d'ADN`, bf$Type)
mod_bf
## F
kruskal.test(f$`Quantité d'ADN`~f$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(f$`Quantité d'ADN`~f$Type, p.adjust.methods= "none")
mod_f=kruskal(f$`Quantité d'ADN`, f$Type)
mod_f
## FY
kruskal.test(fy$`Quantité d'ADN`~fy$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(fy$`Quantité d'ADN`~fy$Type, p.adjust.methods= "none")
mod_fy=kruskal(fy$`Quantité d'ADN`, fy$Type)
mod_fy
## H2O
kruskal.test(h2o$`Quantité d'ADN`~h2o$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(h2o$`Quantité d'ADN`~h2o$Type, p.adjust.methods= "none")
mod_h2o=kruskal(h2o$`Quantité d'ADN`, h2o$Type)
mod_h2o
## Y
kruskal.test(y$`Quantité d'ADN`~y$Type ) #--> p-value: 0.04953 --> effet des CoInocs
kruskalmc(y$`Quantité d'ADN`~y$Type, p.adjust.methods= "none")
mod_y=kruskal(y$`Quantité d'ADN`, y$Type)
mod_y