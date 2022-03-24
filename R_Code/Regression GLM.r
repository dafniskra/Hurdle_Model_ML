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
#########  GLM
###############################################

#### the full model
mod.glm_poisson <- glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM
                        + VehBrand + VehGas + DensityGLM + Region + AreaGLM, 
                        data=learn, offset=log(Exposure), family=poisson())
summary(mod.glm_poisson)  

learn$fitGLM <- fitted(mod.glm_poisson)
test$fitGLM <- predict(mod.glm_poisson, newdata=test, type="response")

# Poisson deviance GLM
Poisson.Deviance(learn$fitGLM, learn$ClaimNb)
Poisson.Deviance(test$fitGLM, test$ClaimNb)


### différence
diff_learn_GLM = ((sum(learn$fitGLM) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_GLM

diff_test_GLM = ((sum(test$fitGLM) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_GLM

### Latex resultats
library(stargazer)
stargazer(mod.glm_poisson, type = "latex")

################" matrice de confusion
cm_learn_GLM = as.matrix(table(Actual = factor(learn$ClaimNb), 
                     Predicted = factor(round(learn$fitGLM, digits = 0)))) # create the confusion matrix
cm_learn_GLM

knitr::kable(cm_learn_GLM, "latex")


cm_test_GLM = as.matrix(table(Actual = factor(test$ClaimNb), 
                     Predicted = factor(round(test$fitGLM, digits = 0)))) # create the confusion matrix
cm_test_GLM
knitr::kable(cm_test_GLM, "latex")



###############################################
#########  GLM Optimisé
###############################################

# Stepwise regression model
step.model <- stepAIC(mod.glm_poisson, direction = "both", trace = FALSE)
summary(step.model)



learn$fitstep <- fitted(step.model)
test$fitstep <- predict(step.model, newdata=test, type="response")


# Poisson deviance GLM
Poisson.Deviance(learn$fitstep, learn$ClaimNb)
Poisson.Deviance(test$fitstep, test$ClaimNb)


### différence
diff_learn_GLM = ((sum(learn$fitstep) - sum(learn$ClaimNb))/sum(learn$ClaimNb))*100
diff_learn_GLM

diff_test_GLM = ((sum(test$fitstep) - sum(test$ClaimNb))/sum(test$ClaimNb))*100
diff_test_GLM

### Latex resultats
#library(stargazer)
stargazer(step.model, type = "latex")

################" matrice de confusion
cm_learn_GLM = as.matrix(table(Actual = factor(learn$ClaimNb), 
                               Predicted = factor(round(learn$fitstep, digits = 0)))) # create the confusion matrix
cm_learn_GLM

knitr::kable(cm_learn_GLM, "latex")


cm_test_GLM = as.matrix(table(Actual = factor(test$ClaimNb), 
                              Predicted = factor(round(test$fitstep, digits = 0)))) # create the confusion matrix
cm_test_GLM
knitr::kable(cm_test_GLM, "latex")