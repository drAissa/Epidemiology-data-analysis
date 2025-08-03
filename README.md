# Epidemiology-data-analysis
# Loading the database 
### Installation des packages
#install.packages("Hmisc")
#install.packages("epiDisplay")
#install.packages("BioStatR")
#install.packages("DescTools") 
### Charger les packages
library(Hmisc)
library(epiDisplay)
library(BioStatR)
library(DescTools)


###### Structure de la base
str(FRCV)

## Conversion des variables  ## as.integer



# transformation pour une variable qualitative
FRCV$Quartier=as.factor(FRCV$Quartier)
FRCV$Sexe=as.factor(FRCV$Sexe)
FRCV$Scolarisation=as.factor(FRCV$Scolarisation)
FRCV$Tabagisme=as.factor(FRCV$Tabagisme)
FRCV$Sedentarité=as.factor(FRCV$Sedentarité)
FRCV$Grades.HTA=as.factor(FRCV$Grades.HTA)
FRCV$Diabete=as.factor(FRCV$Diabete)
FRCV$ATCD.fam.diabete=as.factor(FRCV$ATCD.fam.diabete)
FRCV$Dyspnée=as.factor(FRCV$Dyspnée)
FRCV$Dleur.txq=as.factor(FRCV$Dleur.txq)
FRCV$Flou.visuel=as.factor(FRCV$Flou.visuel)
FRCV$Palpitations=as.factor(FRCV$Palpitations)
FRCV$Dleur.mollet=as.factor(FRCV$Dleur.mollet)
FRCV$Céphalées=as.factor(FRCV$Céphalées)

str(FRCV)

FRCV$Age=as.numeric(FRCV$Age)
FRCV$FC=as.integer(FRCV$FC)
FRCV$PAS=as.numeric(FRCV$PAS)
FRCV$PAD=as.numeric(FRCV$PAD)
FRCV$Glycémie=as.numeric(FRCV$Glycémie)
FRCV$Poids=as.numeric(FRCV$Poids)
FRCV$Taille=as.numeric(FRCV$Taille)
FRCV$Tour.taille=as.numeric(FRCV$Tour.taille) 
FRCV$Tour.hanche=as.numeric(FRCV$Tour.hanche)
#FRCV$Date=as.Date(FRCV$Date, format = "")

## Transformation des modalités
levels(FRCV$Scolarisation) = list("Non"= "0", "Oui"="1")
levels(FRCV$Grades.HTA) = list("grade I"= "1", "grade II"="2", "grade III"="3")
levels(FRCV$Tabagisme) = list("Non"= "0", "Oui"="1")
levels(FRCV$Sedentarité) = list("Non"= "0", "Oui"="1")
levels(FRCV$HTA) = list("Non"= "0", "Oui"="1")
levels(FRCV$Diabete) = list("Non"= "0", "Oui"="1")
levels(FRCV$ATCD.fam.diabete) = list("Non"= "0", "Oui"="1")
levels(FRCV$Dyspnée) = list("Non"= "0", "Oui"="1")
levels(FRCV$Dleur.txq) = list("Non"= "0", "Oui"="1")
levels(FRCV$Flou.visuel) = list("Non"= "0", "Oui"="1")
levels(FRCV$Palpitations) =list("Non"= "0", "Oui"="1")
levels(FRCV$Dleur.mollet) = list("Non"= "0", "Oui"="1")
levels(FRCV$Céphalées) = list("Non"= "0", "Oui"="1")
levels(FRCV$Obésité.abdo) = list("Non"= "0", "Oui"="1")

## Création IMC

FRCV$IMC=FRCV$Poids*10000/(FRCV$Taille*FRCV$Taille)

str(FRCV)



############################## Création Classe glycémie################## Exercice

range(FRCV$Glycémie) ### pour avoir les extrêmes   0.52 - 4.50       ##### Exercice


FRCV$Classes_Glycemie=cut(FRCV$Glycémie,
                          breaks=c(0.50,0.70,1.10,5),
                          include.lowest=TRUE,                  #  [.......[
                          right=FALSE,                       #   ........[
                          labels=c("Hypoglycèmie","Glycémie normale","Hyperglycémie"))   
table(FRCV$Classes_Glycemie)

## Création de la variable diabète dichotomique(Oui/Non)

FRCV$diabete = ifelse(FRCV$Glycémie >= 1.26,"Oui","Non")        ##### Exercice
str(FRCV$diabete)
FRCV$diabete=as.factor(FRCV$diabete)
table(FRCV$diabete)

#diaSco = subset(FRCV, diabete == "Oui" & Scolarisation == "Oui")  ##### Exercice



#### Analyse descriptive

### Parametre dans R pour une variable quantitative

summary(FRCV)  ##### description de la base   

### Age sans données manquantes
### IMC avec 1 donnée manquante

### Moyenne
mean(FRCV$Age)
mean(FRCV$IMC,na.rm = TRUE)

mean(FRCV$Age,na.rm = TRUE)

#### intervalle confiance de la moyenne
MeanCI(FRCV$Age)

###Ecart-Type
sd(FRCV$Age)
sd(FRCV$IMC,na.rm = T)


#### Médiane
median(FRCV$Age)
median(FRCV$IMC,na.rm = T)
#### intervalle confiance de la médiane
MedianCI(FRCV$Age)



###variance
var(FRCV$Age)
var(FRCV$IMC,na.rm = T)

#### Minimum
min(FRCV$Age)
min(FRCV$IMC,na.rm = T)



#### Maximum
max(FRCV$Age)
max(FRCV$IMC,na.rm = T)

###Extreme
range(FRCV$Age)
range(FRCV$IMC,na.rm = T)

###Etendue

diff(range(FRCV$Age))
diff(range(FRCV$IMC,na.rm = T))

### Quantile 

quantile(FRCV$Age)
quantile(FRCV$Age,probs=c(0.25,0.5,0.75))  ### quartile

quantile(FRCV$Age, 0.3333)

quantile(FRCV$Age, probs=c(0.3333,0.6666)) #### tercile

quantile(FRCV$Age, 0.6)


quantile(FRCV$IMC,na.rm = T)

quantile(FRCV$IMC, probs=c(0.3333,0.6666),na.rm = T) #### tercile

FRCV4 = FRCV[,c("Age", "Sexe", "Scolarisation")]


####" Coefficient de variation

cvar(FRCV$Age)
cvar(FRCV$Poids)

#### Synthèse analyse quanti

describe (FRCV$Age)

summary(FRCV$Age)
sd(FRCV$Age)

summ(FRCV$Age)

Desc(FRCV$Age)


#### Analyse descriptive
### Description d'une variable qualitative

## Description
# Mise à plat
summary(FRCV$Sexe)
table(FRCV$Sexe)
table(FRCV$Quartier)
sort(table(FRCV$Quartier), decreasing = T) ### (sort) tri du plus grand au plus petit

### description sexe
describe(FRCV$Sexe)

FRCV$Tabagisme=as.factor(FRCV$Tabagisme)
levels(FRCV$Tabagisme) = list("Non"= "0", "Oui"="1")
describe(FRCV$Tabagisme)

str(FRCV)


library(questionr)
describe(FRCV$Sexe)
freq(FRCV$Sexe)

Desc(FRCV$Sexe)


freq(FRCV$Sexe) ### modalités en ligne
freq(FRCV$Quartier)
freq(FRCV$Quartier,cum = T,total = TRUE,sort = "inc",digits = 1,exclude = NA)

Desc(FRCV$Sexe)
Desc(FRCV$Quartier)

### Analyse d'ensemble

names(FRCV)
quali=FRCV[ ,c("Quartier","Sexe","Scolarisation","Tabagisme","Sedentarité") ]

describe(quali)
quanti = FRCV[,c("Age","IMC","Créatininémie","Uricémie")]


summary(quanti)
sd(FRCV$Age)


################# Graphiques sur R ###########################
library(lessR)

###### Graphiques pour variables quantitatives
## Histogramme
str(FRCV)

hist(FRCV$Taille, xlab = "Taille en cm", ylab = "Fréquence absolue", main = "")

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     col = "green") #couleur en écrit

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     col = 2) #couleur en chiffre

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     breaks = seq(from = 10, to = 100,length = 19), col = "#375D81") #couleur en hexanumérique

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     breaks = seq(from = 10, to = 100,length = 19), col = rainbow(18)) #Palette de couleur1

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     breaks = seq(from = 10, to = 100,length = 19), col = topo.colors(18)) #Palette de couleur2

hist(FRCV$Age, xlab = "Age en années", ylab = "Fréquence absolue", main = "",
     breaks = seq(from = 10, to = 100,length = 19), col = heat.colors(18)) #Palette de couleur3

colors()

Histogram(Age, data = FRCV, xlab = "Age en années", ylab = "Fréquence absolue",
          breaks = 18, fill = "red")

?Histogram


## Boxplot
str(FRCV)

boxplot(FRCV$Taille, col = "lightblue", ylab = "Taille en cm")

#Whisker inférieur = Q1 - 1,5IQR
#Whisker supérieur = Q3 + 1,5IQR

boxplot(FRCV$Age~FRCV$Sexe, xlab = "Sexe", ylab = "Age en année", col = c("pink", "blue"))

Plot(Taille, data = FRCV, vbs_plot = "b")


## Diagramme en bâtons
str(FRCV)

plot(table(FRCV$FC), xlab = "Fréquence cardiaque", ylab = "Fréquence absolue", col = 4)


## Partionnement
par(mfrow=c(1,2))

hist(FRCV$Taille, xlab = "Taille en cm", ylab = "Fréquence absolue", main = "")
plot(table(FRCV$FC), xlab = "Fréquence cardiaque", ylab = "Fréquence absolue", col = 4)

par(mfrow=c(1,1))


###### Graphiques pour variables quanlitatives
## Diagramme en barres
str(FRCV)

# Diagramme en barres verticales
barplot(table(FRCV$Quartier), col = c(1,2,3,4),
        xlab = "Quartier", 
        ylab = "Fréquence absolue")

BarChart(Quartier, data = FRCV)

# Diagramme en barres horizontales
barplot(table(FRCV$Quartier), col = c(1,2,3,4),
        xlab = "Quartier", 
        ylab = "Fréquence absolue",
        horiz = T)

BarChart(Quartier, data = FRCV, horiz = T)

# Diagramme superposé 
barplot(table(FRCV$Sexe, FRCV$Quartier),
        xlab = "Quartier", 
        ylab = "Fréquence absolue")

BarChart(Quartier, by = Sexe, data = FRCV)

# Diagramme côte à côte
barplot(table(FRCV$Sexe, FRCV$Quartier), beside = T,
        xlab = "Quartier", 
        ylab = "Fréquence absolue")

BarChart(Quartier, by = Sexe, data = FRCV, beside = T)


## Diagramme circulaire
pie(table(FRCV$Quartier), clockwise = T)

pie(sort(table(FRCV$Quartier), decreasing = T), clockwise = T, col = c(1,2,3,4)) # Ordre décroissant


## Graphique croisement 2 variables qualitatives
Plot(Scolarisation, Sexe, data = FRCV)

## Graphique entre 2 variables quantitatives
plot(FRCV$Age, FRCV$Poids, xlab = "Age", ylab = "Poids", col = 4, pch = 2) #R base

Plot(Age, Poids, data = FRCV) #lessR

################ Fin de graphique sur R #######################


################ Début comparaison de 2 moy et de 2 prop ######################

#####Insuffissance rénale

str(FRCV)
#Calculer la clairance de la Creat avec la formule de Cockcroft
FRCV$Kc = ifelse(FRCV$Sexe=="Masculin", 1, .85)
str(FRCV$Kc) 
FRCV$NumerateurC = FRCV$Kc * (140 - FRCV$Age) * FRCV$Poids
FRCV$DenominateurC = 7.2 * FRCV$Créatininémie         
FRCV$Cockcroft = FRCV$NumerateurC / FRCV$DenominateurC
describe(FRCV$Cockcroft)      
summary(FRCV$Cockcroft) 

#Calculer la clairance avec la formule MDRD
FRCV$Km = ifelse(FRCV$Sexe=="Masculin", 1, .742)
FRCV$Creat10 = FRCV$Créatininémie/ 10
FRCV$MDRD = FRCV$Km * 186.3 * (FRCV$Creat10^-1.154) * (FRCV$Age^-0.203) * 1.21
describe(FRCV$MDRD)   
summary(FRCV$MDRD) 

#Créer la variable Classes d'IR
FRCV$Classes_IR = cut(FRCV$MDRD,
                      breaks = c(0, 15, 30, 60, 90, 260),
                      right=F,
                      labels = c("IR terminale", "IR severe",
                                 "IR moderee", "IR debutante",
                                 "Absence d'IR"))
describe(FRCV$Classes_IR)



#Créer la variable Classes d'IR
FRCV$Classes_IR1 = cut(FRCV$Cockcroft,
                       breaks = c(0, 15, 30, 60, 90, 260),
                       right=F,
                       labels = c("IR terminale", "IR severe",
                                  "IR moderee", "IR debutante",
                                  "Absence d'IR"))
describe(FRCV$Classes_IR1)


#####     ANALYTIQUE

#### comparaison de proportions  QUALI - QUALI

str(FRCV)
FRCV$Obesite=as.factor(FRCV$Obesite)

library(epiDisplay)


tabpct(FRCV$Sexe,FRCV$Obesite)         ###tableau de contigence pour déterminer les proportions à comparer

FRCV$Obesite=relevel(FRCV$Obesite,ref="Oui")  #### Changement de référence de la variable dépendante

tabpct(FRCV$Sexe,FRCV$Obesite)

chisq.test(FRCV$Sexe,FRCV$Obesite)$expected #### les valeurs attendues ou théoriques ## toutes les attendues sont supérieures à 5

chisq.test(FRCV$Sexe,FRCV$Obesite,correct = FALSE) ## Chi2 non corrigé de Pearson p-value < 0.001

cc(FRCV$Sexe,FRCV$Obesite)

FRCV$Sexe=relevel(FRCV$Sexe,ref="Feminin")

cc(FRCV$Sexe,FRCV$Obesite) ### Calcul du OR
cs(FRCV$Sexe,FRCV$Obesite) ### Calcul du RR


tabpct(FRCV$Tabagisme,FRCV$Diabete) 
chisq.test(FRCV$Tabagisme,FRCV$Diabete)$expected
chisq.test(FRCV$Tabagisme,FRCV$Diabete,correct =F)
cc(FRCV$Tabagisme,FRCV$Diabete)


FRCV$Diabete=relevel(FRCV$Diabete,ref="Oui")
FRCV$Tabagisme=relevel(FRCV$Tabagisme,ref="Non")
cc(FRCV$Tabagisme,FRCV$Diabete)


#################### Comparaison de deux moyennes dans R  ###
#Relation entre HTA et Creatininemie
FRCV$HTA=as.factor(FRCV$HTA)

## détermination des moyennes
str(FRCV)

mean(FRCV$Créatininémie[FRCV$HTA=="Non"]) #### Moyenne chez les non hypertendus
mean(FRCV$Créatininémie[FRCV$HTA=="Oui"]) #### Moyenne chez les  hypertendus

tapply(FRCV$Créatininémie,FRCV$HTA, mean)  ## Synthése moyenne


mean(FRCV$Créatininémie[FRCV$HTA=="Oui"])

# Etude la normalite de la distribution de Creatininemie dans les strates d'HTA
## méthode graphique

par(mfrow=c(1,2))


qqnorm(FRCV$Créatininémie[FRCV$HTA=="0"],main="Non hypertendus")
qqline(FRCV$Créatininémie[FRCV$HTA=="0"],col="red")

qqnorm(FRCV$Créatininémie[FRCV$HTA=="1"],main="Hypertendus")
qqline(FRCV$Créatininémie[FRCV$HTA=="1"],col="red")


qqnorm(FRCV$Créatininémie[FRCV$HTA=="Non"],main="Non hypertendus")
qqline(FRCV$Créatininémie[FRCV$HTA=="Non"],col="red")

qqnorm(FRCV$Créatininémie[FRCV$HTA=="Oui"],main="Hypertendus")
qqline(FRCV$Créatininémie[FRCV$HTA=="Oui"],col="red")

qqnorm(FRCV$Créatininémie)

hist(FRCV$Créatininémie[FRCV$HTA=="Non"],main="Non hypertendus")
hist(FRCV$Créatininémie[FRCV$HTA=="Oui"],main="Hypertendus")

boxplot(FRCV$Créatininémie)

boxplot(FRCV$Créatininémie~FRCV$HTA)

### etude de la normalité par le test de shapiro seuil 10%
## H0 : distibution observée=distibution de la population de référence (Distribution normale)
## H1 : distibution observée différente de la distibution de la population de référence (Distribution anormale)

shapiro.test(FRCV$Créatininémie[FRCV$HTA=="Non"])
shapiro.test(FRCV$Créatininémie[FRCV$HTA=="Oui"])
shapiro.test(FRCV$Créatininémie)

### Distibution non normale donc faire test non paramétrique de wilcox

wilcox.test(FRCV$Créatininémie~FRCV$HTA)

## résumé 
tapply(FRCV$Créatininémie,FRCV$HTA, mean)
shapiro.test(FRCV$Créatininémie)
wilcox.test(FRCV$Créatininémie~FRCV$HTA) ### p-value = 0.00001657


#### Supposons que la distribution est normale céd shapiro.test supérieur é 10

bartlett.test(FRCV$Créatininémie~FRCV$HTA)   ##### p-value = 0.01881 # variance inéguale
t.test (FRCV$Créatininémie~FRCV$HTA, var.equal=F) ### p-value = 0.000044

###si Test de Bartlett était supérieur à 5% alors on devrait faire le test de student
t.test (FRCV$Créatininémie~FRCV$HTA, var.equal=T) ### p-value = 0.00003974


#### Synthése et Démarche à suivre

######### Si test de Shapiro-Wilk > à 10% alors distribution normale faire test paramétrique (t-test)
## Si distribution normale faire l'homogénéité des variance avec le test de Bartlett
# Si test bartlett.test > à 5% (variance égale) faire le test t de student : t.test (FRCV$var1~FRCV$var2, var.equal=T) 
# Si test bartlett.test < à 5% (variance inégale) faire le test de welch     : t.test (FRCV$var1~FRCV$var2, var.equal=F) 

######## Si test de Shapiro-Wilk inférieur à 10% alors distribution non-normale faire test non-paramétrique (Test de Wilcoxon-Mann-Whitney :wilcox.test)

################ Fin comparaison de moy et de prop ######################


############## Début de comp entre 3 moy ou plus et corrélation ###############

### Comparaison de 3 moyennes ou plus
str(FRCV)

# Visualiser les moyennes à comparer
tapply(FRCV$Age, FRCV$Grades.HTA, mean)

### Nombre de comparaison 2 à 2 : n(n-1)/2


## Vérification des critères d'applicabilité
boxplot(FRCV$Age~FRCV$Grades.HTA, xlab = "Grades HTA", ylab = "Age en années",
        col = c("blue","red","yellow"))

test = aov(FRCV$Age~FRCV$Grades.HTA)

## Normalité de la distribution
shapiro.test(test$residuals) # Pas de distribution normale


# Si pas de distribution normale, test de Kruskal Wallis
kruskal.test(FRCV$Age~FRCV$Grades.HTA)

## Comparaison 2 à 2 si test significatif
TukeyHSD(test)

#### Résumé : Si les critères remplis à moitiè
# Distribution normale
# Test de barlett mais pas d'homogénéité des variances
## Dans ce cas Kruskal Wallis


#### Résumé : Si tous les critères sont remplis
# Distribution normale
# Test de barlett avec d'homogénéité des variances
## Dans ce cas ANOVA
summary(test)

summary(aov(FRCV$Age~FRCV$Grades.HTA))

# Après ANOVA vous faites le test de Tukey : TukeyHSD(test)


###### Corrélation linéaire simple
str(FRCV)

## Relation entre Age et le poids

# Visualisation
library(lessR)

Plot(Age, Poids, data = FRCV, fit = "lm")


## Normalité de la distribution
par(mfrow=c(1,2))

qqnorm(FRCV$Age,col="blue",main = "Age")
qqnorm(FRCV$Poids,col="blue",main = "Poids")

shapiro.test(FRCV$Age) #Distribution pas normale
shapiro.test(FRCV$Poids) #Distribution pas normale

### Comme les 2 distributions ne sont pas normales
cor.test(FRCV$Age, FRCV$Poids, method = "spearman") #rho

### Les 2 distributions ne sont pas normales et trop d'ex aequo lors du test de spearman
cor.test(FRCV$Age, FRCV$Poids, method = "kendall") #tau


### L'une des distributions ou les 2 sont normales
cor.test(FRCV$Age, FRCV$Poids, method = "pearson") #cor

############### Fin de comp 3 moy ou plus et correléation #################

######################### Cloture de la session R ##########################
