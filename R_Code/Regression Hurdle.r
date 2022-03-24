#########  load packages and data
require(MASS)
require(stats)
library(data.table)
library(plyr)
library(rpart)
library(rpart.plot)

#read the data 
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
#########  Model Hurdle
###############################################
#the full model
mod.hurdle <- hurdle(ClaimNb ~  VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM
                     + VehBrand + VehGas + DensityGLM + Region + AreaGLM | VehPowerGLM + VehAgeGLM + 
                       DrivAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region + AreaGLM, 
                     data = learn, offset=log(Exposure), dist = "poisson", 
                     zero.dist = "binomial")

summary(mod.hurdle)  

### Latex resultats regression
stargazer(mod.hurdle, type = "latex")
stargazer(mod.hurdle, type = "latex", zero.component = T)



learn$fitHurdle <- fitted(mod.hurdle)
test$fitHurdle <- predict(mod.hurdle, newdata=test, type="response")

# Poisson deviance Hurdle
Poisson.Deviance(learn$fitHurdle, learn$ClaimNb)
Poisson.Deviance(test$fitHurdle, test$ClaimNb)


### différence
diff_learn_Hurdle = ((sum(learn$fitHurdle) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_Hurdle

diff_test_Hurdle = ((sum(test$fitHurdle) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_Hurdle

################" matrice de confusion
cm_learn_Hurdle = as.matrix(table(Actual = factor(learn$ClaimNb), 
                                  Predicted = factor(round(learn$fitHurdle, digits = 0)))) # create the confusion matrix
cm_learn_Hurdle

knitr::kable(cm_learn_Hurdle, "latex")

cm_test_Hurdle = as.matrix(table(Actual = factor(test$ClaimNb), 
                                 Predicted = factor(round(test$fitHurdle, digits = 0)))) # create the confusion matrix
cm_test_Hurdle
knitr::kable(cm_test_Hurdle, "latex")

###############################################
#########  Model Hurdle optimisé
###############################################

# Stepwise regression model
step.model_H <- stepAIC(mod.hurdle, direction = "both", trace = FALSE)
summary(step.model_H)

learn$fitsteH <- fitted(step.model_H)
test$fitsteH <- predict(step.model_H, newdata=test, type="response")


# Poisson deviance GLM
Poisson.Deviance(learn$fitsteH, learn$ClaimNb)
Poisson.Deviance(test$fitsteH, test$ClaimNb)


### différence
diff_learn_GLM = ((sum(learn$fitsteH) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_GLM

diff_test_GLM = ((sum(test$fitsteH) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_GLM

### Latex resultats
#library(stargazer)
stargazer(step.model_H, type = "latex")

################" matrice de confusion
cm_learn_GLM = as.matrix(table(Actual = factor(learn$ClaimNb), 
                               Predicted = factor(round(learn$fitsteH, digits = 0)))) # create the confusion matrix
cm_learn_GLM

knitr::kable(cm_learn_GLM, "latex")


cm_test_GLM = as.matrix(table(Actual = factor(test$ClaimNb), 
                              Predicted = factor(round(test$fitsteH, digits = 0)))) # create the confusion matrix
cm_test_GLM
knitr::kable(cm_test_GLM, "latex")

#latex
stargazer(step.model_H, type = "latex")
stargazer(step.model_H, type = "latex", zero.component = T)
