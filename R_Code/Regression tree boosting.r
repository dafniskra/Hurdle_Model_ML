#########  load packages and data
require(MASS)
require(stats)
library(data.table)
library(plyr)
library(rpart)
library(rpart.plot)

learn <- read.csv("learn_total.csv",header=TRUE, stringsAsFactors = TRUE)
test <- read.csv("test_total.csv",header=TRUE, stringsAsFactors = TRUE)

fonction_clean_data_1 <- function(dat) {
  dat$VehGas <- factor(dat$VehGas)     
  dat$n <- 1                            
  dat$ClaimNb <- pmin(dat$ClaimNb, 4)   
  dat$Exposure <- pmin(dat$Exposure, 1) 
  str(dat)
  return(dat)
}

learn <- fonction_clean_data_1(learn)
test <- fonction_clean_data_1(test)

#########  Poisson deviance statistics pour comparer les résultats entre eux. 
######### C'est la métrique la plus utilisé dans le domaine des donnes de comptage. 

Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}


#########  feature pre-processing
fonction_clean_data_2 <- function(dat2) {
  dat2$AreaRT <- as.integer(dat2$Area)
  dat2$VehGasRT <- as.integer(dat2$VehGas)
  str(dat2)
  return(dat2)
}

learn <- fonction_clean_data_2(learn)
test <- fonction_clean_data_2(test)


###############################################
#########  Poisson regression tree boosting
###############################################

### Model PBM3
J0 <- 3       # depth of tree
M0 <- 50      # iterations
nu <- 1       # shrinkage constant 

learn$fitPBM <- learn$Exposure
test$fitPBM  <- test$Exposure

for (m in 1:M0){
    PBM.1 <- rpart(cbind(fitPBM,ClaimNb) ~ AreaRT + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGasRT + Density + Region, 
             data=learn, method="poisson",
             control=rpart.control(maxdepth=J0, maxsurrogate=0, xval=1, minbucket=10000, cp=0.00001))     
             if(m>1){
                   learn$fitPBM <- learn$fitPBM * predict(PBM.1)^nu
                   test$fitPBM <- test$fitPBM * predict(PBM.1, newdata=test)^nu
                   } else {
                   learn$fitPBM <- learn$fitPBM * predict(PBM.1)
                   test$fitPBM <- test$fitPBM * predict(PBM.1, newdata=test)
                   }
              }

# Poisson deviance PBM
100*Poisson.Deviance(learn$fitPBM, learn$ClaimNb)
100*Poisson.Deviance(test$fitPBM, test$ClaimNb)


################" matrice de confusion
cm_learn_PBM = as.matrix(table(Actual = factor(learn$ClaimNb), 
                               Predicted = factor(round(learn$fitPBM, digits = 0)))) # create the confusion matrix
cm_learn_PBM

knitr::kable(cm_learn_PBM, "latex")

cm_test_PBM = as.matrix(table(Actual = factor(test$ClaimNb), 
                              Predicted = factor(round(test$fitPBM, digits = 0)))) # create the confusion matrix
cm_test_PBM
knitr::kable(cm_test_PBM, "latex")

### différence
diff_learn_PBM = ((sum(learn$fitPBM) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_PBM

diff_test_PBM = ((sum(test$fitPBM) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_PBM