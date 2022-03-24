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


#########  feature pre-processing for GLM
fonction_clean_data_2 <- function(dat2) {
  dat2$AreaGLM <- as.integer(dat2$Area)
  dat2$VehPowerGLM <- as.factor(pmin(dat2$VehPower,9))
  VehAgeGLM <- cbind(c(0:110), c(1, rep(2,10), rep(3,100)))
  dat2$VehAgeGLM <- as.factor(VehAgeGLM[dat2$VehAge+1,2])
  dat2[,"VehAgeGLM"] <-relevel(dat2[,"VehAgeGLM"], ref="2")
  DrivAgeGLM <- cbind(c(18:100), c(rep(1,21-18), rep(2,26-21), rep(3,31-26), rep(4,41-31), rep(5,51-41), rep(6,71-51), rep(7,101-71)))
  dat2$DrivAgeGLM <- as.factor(DrivAgeGLM[dat2$DrivAge-17,2])
  dat2[,"DrivAgeGLM"] <-relevel(dat2[,"DrivAgeGLM"], ref="5")
  dat2$BonusMalusGLM <- as.integer(pmin(dat2$BonusMalus, 150))
  dat2$DensityGLM <- as.numeric(log(dat2$Density))
  dat2[,"Region"] <-relevel(dat2[,"Region"], ref="R24")
  dat2$AreaRT <- as.integer(dat2$Area)
  dat2$VehGasRT <- as.integer(dat2$VehGas)
  str(dat2)
  return(dat2)
}

learn <- fonction_clean_data_2(learn)
test <- fonction_clean_data_2(test)


###############################################
#########  Regressionn tree analysis
###############################################

### Model RT2
tree1 <- rpart(cbind(Exposure,ClaimNb) ~ AreaRT + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGasRT + Density + Region, 
            learn, method="poisson",
            control=rpart.control(xval=1, minbucket=10000, cp=0.00001))     

learn$fitRT <- predict(tree1)*learn$Exposure
test$fitRT <- predict(tree1, newdata=test)*test$Exposure

# Poisson deviance RT1
Poisson.Deviance(learn$fitRT, learn$ClaimNb)
Poisson.Deviance(test$fitRT, test$ClaimNb)

# prune to appropriate cp constant
printcp(tree1)
tree2 <- prune(tree1, cp=0.00003)
printcp(tree2)

learn$fitRT2 <- predict(tree2)*learn$Exposure
test$fitRT2 <- predict(tree2, newdata=test)*test$Exposure


# Poisson deviance RT2
Poisson.Deviance(learn$fitRT2, learn$ClaimNb)
Poisson.Deviance(test$fitRT2, test$ClaimNb)


################" matrice de confusion
cm_learn_RT2 = as.matrix(table(Actual = factor(learn$ClaimNb), 
                               Predicted = factor(round(learn$fitRT2, digits = 0)))) # create the confusion matrix
cm_learn_RT2

knitr::kable(cm_learn_RT2, "latex")

cm_test_RT2 = as.matrix(table(Actual = factor(test$ClaimNb), 
                              Predicted = factor(round(test$fitRT2, digits = 0)))) # create the confusion matrix
cm_test_RT2
knitr::kable(cm_test_RT2, "latex")


### différence
diff_learn_RT2 = ((sum(learn$fitRT2) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_RT2

diff_test_RT2 = ((sum(test$fitRT2) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_RT2

