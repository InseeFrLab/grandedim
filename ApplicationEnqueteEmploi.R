### Application empirique double selection: Niveau de diplôme et salaire, enquête emploi
### Jérémy L'Hour
### 29/05/2020

### CHARGEMENT DES PACKAGES
library('haven')
library('glmnet')
library('grplasso')


#######################
#######################
### DATA MANAGEMENT ###
#######################
#######################

data = read_sas("indiv171.sas7bdat")
data = as.data.frame(data)

data[,"SALRED"] = as.numeric(data[,"SALRED"]) # salaire mensuel net
data[,"LOG_SAL"] = log(data[,"SALRED"]) # log salaire

# Variable d'intérêt, "X_1", en facteurs à plusieurs niveaux
data[,"DIP"] = ifelse(data[,"DIP"]=="",NA,data[,"DIP"])
data[,"DIP"] = as.factor(data[,"DIP"]) # niveau de diplome le plus eleve


data[,"EXTRIDF"] = as.numeric(data[,"EXTRIDF"]) # poids de sondages

# Autres variables "X_2"
data[,"SANTGEN"] = as.factor(data[,"SANTGEN"]) # niveau de santé perçu
data[,"AG"] = as.numeric(data[,"AG"])


### Mise en place des bonnes matrices
outcome = "LOG_SAL"
X_1_names = "DIP"
X_2_names = c("SANTGEN","AG")

data_use = data[complete.cases(data[,c(outcome,X_1_names,X_2_names)]),]

Y = data_use[,outcome]
X_2 = makeX(data_use[,X_2_names])
X_1 = makeX(data.frame("EDUC"=data_use[,X_1_names]))

############################################
############################################
### ETAPE 1: Selection par rapport à "Y" ###
############################################
############################################

# Il s'agit d'une régression Lasso classique

n = nrow(X_2); p = ncol(X_2)
gamma_pen = .1/log(max(p,n))
lambda = 1.1*qnorm(1-.5*gamma_pen/p)/sqrt(n) # niveau (theorique) de penalisation Lasso

outcome_selec = glmnet(X_2,Y, family="gaussian",alpha=1,lambda=lambda)
predict(outcome_selec,type="coef")

set_Y = predict(outcome_selec,type="nonzero") # ensemble des coefficients non nuls à cette étape

##################################################################
##################################################################
### ETAPE 2: Selection par rapport à "X_1" avec le group Lasso ###
##################################################################
##################################################################

# Cette seconde étape est plus compliquée: la variable X_1 possèdre plusieurs modalités,
# Il faut donc binariser (les convertir en one-hot) et faire autant de régressions qu'il y a de modalités-1
# On propose une approche Group-Lasso dans la mesure où l'on suppose que le schéma de sparsité est le même pour toutes ces régressions.
# Cela nécessite de faire des régression empiler et de vectoriser la variable dépendante.
# Pour le Group Lasso il y a donc p groupes de variables

X_1_vec = matrix(c(X_1), ncol=1)
X_2_vec =  kronecker(diag(ncol(X_1)), X_2)
group_index = rep(1:p,ncol(X_1))

immunization_selec = grplasso(X_2_vec, X_1_vec, group_index, lambda=n*lambda, model=LinReg()) # ici la fonction objectif est differente, il faut multiplier la penalité par n
Gamma_hat = matrix(immunization_selec$coefficients, ncol=ncol(X_1))
row.names(Gamma_hat) = colnames(X_2)

set_X1 = c(which(apply(Gamma_hat>0,1,sum)>0))

#############################
#############################
### ETAPE 3: Etape finale ###
#############################
#############################

S_hat = sort(unique(unlist(union(set_Y,set_X1))))

dbs_reg = lm(Y ~ X_1 + X_2[,S_hat])

coef_names = paste("X_1",colnames(X_1),sep="")
tau_hat = dbs_reg$coefficients[coef_names]

### Calcul de l'écart-type
Gamma_hat = solve(t(X_2[,S_hat])%*%X_2[,S_hat]) %*% (t(X_2[,S_hat]) %*% X_1) # Regression post-lasso de chaque modalités de X_1
treat_residuals = X_1 - X_2[,S_hat] %*% Gamma_hat

M_matrix = sweep(t(treat_residuals),MARGIN=2,dbs_reg$residuals,`*`) %*% t(sweep(t(treat_residuals),MARGIN=2,dbs_reg$residuals,`*`))
C_matrix = t(treat_residuals)%*%treat_residuals
sigma = solve(C_matrix) %*% M_matrix %*% solve(C_matrix)

# Bien regarder les histoires d'eliminer une dummy etc.

# Finalement: plot
plot_data = ts(cbind(tau_hat + qnorm(0.025)*diag(sigma), tau_hat, tau_hat + qnorm(0.975) *diag(sigma)))

plot(plot_data, plot.type="single",
     col=c("firebrick","firebrick","firebrick"),
     lwd=c(1,2,1),
     lty=c(6,1,6))

dip = cbind(c("10","12","22","21","30","31","32","33","41","42","43","44","50","60","70","71"),
            c("Master (recherche ou professionnel), DEA, DESS, Doctorat",
              "Ecoles niveau licence et au-delà",
              "Maîtrise (M1)",
              "Licence (L3)",
              "DEUG",
              "DUT, BTS",
              "Autre diplôme (niveau bac+2)",
              "Paramédical et social (niveau bac+2)",
              "Baccalauréat général",
              "Bac technologique",
              "Bac professionnel",
              "Brevet de technicien, brevet professionnel",
              "CAP, BEP",
              "Brevet des collèges",
              "Certificat d'études primaires",
              "Sans diplôme"))
colnames(dip) = c("ID","Description")

## reste à calculer ecart-type etc