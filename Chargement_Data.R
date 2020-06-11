### Données enquête emploi pour la formation
### 10/06/2020
### Jérémy L'Hour

rm(list=ls())

### CHARGEMENT DES PACKAGES
library('aws.s3')
library('glmnet')
library('fastDummies') # pour créer des dummies à partir de catégories
library('caret') # pour utiliser la fonction qui repère les variables colinéaires

setwd("/home/zctxti")
#setwd("/Users/jeremylhour/Documents/code")

###########################
###########################
### GESTION DES DONNEES ###
###########################
###########################

bucket = "groupe-1006"
files = get_bucket(bucket = bucket, prefix = "grandedim/")
save_object(bucket = bucket,  prefix = "grandedim/", object="grandedim/data_ee.Rda" ,file="grandedim/data_ee.Rda")

load("grandedim/data_ee.Rda")

data_use = data_ee

# Label des variabesl utilisées
outcome = "LOG_SAL"
X_1_names = "DIP"
names_continuous = c("AG", "AG_2", "ANCENTR","HHC","NBENFIND")
names_categorical = c("SEXE","APPDIP","SANTGEN","ADMHAND","CATAU2010", "CHPUB","CHRON",
                      "COMSAL","COURED","CSPM","CSPP","FORDAT","DESC","IMMI","DUHAB","ENFRED","SPE",
                      "MAISOC","MATRI","NAT14","NBAGEENFA","NBENFA1","NBENFA10","NBENFA15","NBENFA18","QP","REG","SO","SOIRC","TYPMEN21")
X_2_names = c(names_continuous,names_categorical)

# "Y" (outcome)
Y = data_use[,outcome]

# "X_1" (variables d'intérêt)
X_1 = model.matrix(~. - 1, data = data.frame("EDUC"=as.factor(data_use[,X_1_names])), contrasts.arg = "EDUC")
X_1 = X_1[,1:(ncol(X_1)-1)] # On enlève la modalité "sans diplôme" pour éviter les problèmes de colinéarité.

# "X_2" (contrôles)
one_hot_category = dummy_cols(data_use[,names_categorical], remove_most_frequent_dummy=TRUE, remove_selected_columns=TRUE) # on transforme les variables catégorielles en variables binaires
X_2 = as.matrix(cbind(data_use[, names_continuous], one_hot_category))
X_2 = X_2[,!duplicated(t(X_2))] # On enlève les colonnes dupliquées
colinear = caret::findLinearCombos(cbind(X_1,X_2,rep(1,nrow(X_2)))) 
suppr = colinear$remove-ncol(X_1) # recalage par rapport à l'indice de X_2
X_2 = X_2[,-suppr] # On enlève les colonnes qui créent de la multicolinéairité, avec l'inclusion de X_1 et une constante

# Identifiants clustering
ID_menage = data_use[,"IDENT"] # Identifiant du ménage, pour cluster dans les écart-types.

coef_names = paste("X_1",colnames(X_1),sep="") # nom des variables d'intérêt
n = nrow(X_2); p = ncol(X_2)

remove(data_use, one_hot_category)

#############################
#############################
### Exemple de régression ###
#############################
#############################

### Régression simple
reg_simple = lm(Y ~ X_1)
summary(reg_simple)
tau_simple = reg_simple$coefficients[coef_names]


