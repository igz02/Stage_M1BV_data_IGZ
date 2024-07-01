library("ggpubr")
library("devtools")
library("ggplot2")
library("ggpubr")
library(dplyr)

#Corrélation entre la Note et la quantité d'ADN d'Abra pour CoInoc 
dna_symp <- read.csv("ADN_Note_CoInoc.csv", header = TRUE, sep = ";", dec = ",", na.strings = "NA", stringsAsFactors = FALSE)

dna_symp$Condition <- as.factor(dna_symp$Condition)

ggscatter(dna_symp, x = "ADN" , y = "Note", xlab = "Quantité d'ADN (en ng/microL)",
          ylab = "Note",
          add = "reg.line",                         # Add regression line
          conf.int = TRUE,                          # Add confidence interval
          conf.int.level = 0.95,
          add.params = list(color = "black",
                            size = 1,
                            linetype = 1,
                            fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.sep = "\n"), # Correlation and probability value
          color = "Condition",                       # Custom color palettes
          palette = c(rainbow),
          ggtheme = theme_bw()) #+ stat_cor(aes(color = "Condition"), label.x = 0.3)                      # Customize themes
#+ color = "Condition", palette = "jco", shape = "Condition")
           # Color by groups "cyl"
                           # Change point shape by groups "cyl"           # Add correlation coefficient

#Corrélation entre la quantité d'ADN pour CoInoc et quantité d'ADN pour SynCom
library(readxl)

adn_cor <- read_excel("ADN_SynCom_CoInoc_Correl.xlsx", 
                      na = "NA")
adn_cor$Condition <- as.factor(adn_cor$Condition)
ggscatter(adn_cor, x = "CoInoc" , y = "SynCom", xlab = "Quantité d'ADN présente sur CoInoc (en ng/microL)",
          ylab = "Quantité d'ADN présente sur SynCom (en ng/microL)",
          add = "reg.line",                         # Add regression line
          conf.int = TRUE,                          # Add confidence interval
          conf.int.level = 0.95,
          add.params = list(color = "black",
                            size = 1,
                            linetype = 1,
                            fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.sep = "\n"), # Correlation and probability value
          color = "Condition",                       # Custom color palettes
          palette = c(rainbow),
          ggtheme = theme_bw())
#Corrélation entre poids frais et Indice de maladie
poids_ind <- read_excel("Correlation_Poids_Indice.xlsx")

cor_test <- cor.test(poids_ind$`Poids frais`, poids_ind$`Indice de maladie`, method = "spearman")
cor_test
