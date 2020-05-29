### Application empirique double selection: Niveau de diplôme et salaire, enquête emploi
### Jérémy L'Hour
### 29/05/2020

### CHARGEMENT DES PACKAGES
library('haven')
library('glmnet')
library('grplasso')


data = read_sas("grandedim/indiv171.sas7bdat")
data = as.data.frame(data)

### DATA MANAGEMENT ###
data[,"SALRED"] = as.numeric(data[,"SALRED"]) # salaire mensuel net
data[,"LOG_SAL"] = log(data[,"SALRED"]) # log salaire

# Variable d'intérêt, "X_1", en facteurs à plusieurs niveaux
data[,"DIP"] = as.factor(data[,"DIP"]) # niveau de diplome le plus eleve

data[,"EXTRIDF"] = as.numeric(data[,"EXTRIDF"]) # poids de sondages

# Autres variables "X_2"
data[,"SANTGEN"] = as.factor(data[,"SANTGEN"]) # niveau de santé perçu
data[,"AG"] = as.numeric(data[,"AG"])


summary(lm(LOG_SAL ~ DIP + SANTGEN + AG, data=data))

### Mise en place des bonnes matrices
outcome = "LOG_SAL"
X_1_names = "DIP"
X_2_names = c("SANTGEN","AG")

data_use = data[complete.cases(data[,c(outcome,X_1_names,X_2_names)]),]

Y = data_use[,outcome]
X_2 = makeX(data_use[,X_2_names])
X_1 = makeX(as.data.frame(data_use[,X_1_names]))


### ETAPE 1: Selection par rapport à "Y"
n = nrow(X_2); p = ncol(X_2)
gamma_pen = .1/log(max(p,n))
lambda = 1.1*qnorm(1-.5*gamma_pen/p)/sqrt(n) # niveau (theorique) de penalisation Lasso

outcome_selec = glmnet(X_2,Y, family="gaussian",alpha=1,lambda=lambda)
predict(outcome_selec,type="coef")

set_Y = predict(outcome_selec,type="nonzero") # ensemble des coefficients non nuls à cette étape

### ETAPE 2: Selection par rapport à "X_1" avec le group Lasso
# Attention, il faut faire une regression empilee et donc vectorise la variable dependante
X_1_vec = matrix(c(X_1), ncol=1)
X_2_vec =  kronecker(diag(ncol(X_1)), X_2)
group_index = rep(1:p,ncol(X_1))

immunization_selec = grplasso(X_2_vec, X_1_vec, group_index, lambda=n*lambda, model=LinReg()) # ici la fonction objectif est differente, il faut multiplier la penalité par n
Gamma_hat = matrix(immunization_selec$coefficients, ncol=ncol(X_1))
row.names(Gamma_hat) = colnames(X_2)

set_X1 = c(which(apply(Gamma_hat>0,1,sum)>0))

### ETAPE 3: Etape finale
S_hat = sort(unique(unlist(union(set_Y,set_X1))))

dbs_reg = lm(Y ~ X_1 + X_2[,S_hat])

## reste à calculer ecart-type etc