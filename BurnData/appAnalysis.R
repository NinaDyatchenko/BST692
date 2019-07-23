
# Nina Dyatchenko
# BST692 Final
# appAnalysis for Burndata Shiny App


# Loading packages
library(aplore3)  # contains data1000
library(caret)
library(randomForest)
library(C50)
library(tidyverse)
library(caret)
library(class)
library(rattle)	
library(party)
library(gmodels)

# teh Data source
data(burn1000)

# Normalizing data to be in range from 0 to 1

normalize <- function(x){
  return((x-min(x))/(max(x)- min(x)))
}


#### Creating Training and Test datasets:
set.seed(100)  # setting seed for foxed sampling

burnNorm <- burn1000 %>% 
  map(., as.numeric) %>% 
  map(., normalize) %>% # normalize all variables
  as.data.frame()

summary(burnNorm$age)

# 80% of the subjects go into a train dataset
trainingSet <- unlist(caret::createDataPartition(burn1000$death, p = 0.80))
trainingSet

burn_train <- burnNorm[trainingSet,]

# subject sthat didn't go into train dataset go to test dataset
burn_test <- burnNorm[-trainingSet, ]

burn_train1 <- burnNorm[trainingSet,]

burn_test2 <- burnNorm[-trainingSet, ]

burn_train1_isdead <- burn_train1$death
burn_test2_isdead <- burn_test2$death
burn_train1 <-  burn_train1 %>% select(-death)
burn_test2 <-  burn_test2 %>% select(-death)


###### Creating KNN
burn_test_pred <- knn(train = burn_train1, 
                      test = burn_test2, 
                      cl = burn_train1_isdead,  # true class of training
                      k = 21)
# only way to tune it is to change K
# Evaluate KNN prediction model:

CrossTable(x = burn_test2_isdead, y = burn_test_pred, prop.chisq = FALSE)
# 7 false negatives from 200 people
# 23/30 accuracy
# 77% accuracy

##### End of KNN

### Making Logistic regression
glm.fit <- glm(death ~ tbsa + inh_inj + age + gender + flame + race, 
               family = binomial, 
               data = burn_train)
glm.fit

glm.prediction<- predict(glm.fit, burn_test, type = "response")
glm.prediction

# predict glm on train, then predict on test, then assign alive/dead with if statement
# check with confusionmatrix
# then get the kappa

predicted <- as.factor(dplyr::if_else(glm.prediction <= .85, 0, 1))
predicted

truth <- as.factor(burn_test$death)


# Evaluate GLM prediction model:
caret::confusionMatrix(predicted, 
                       truth, 
                       positive = "1")
# Kappa is 0.5141




burn_test2_isdead1 <- factor(burn_test2_isdead)

# C5.0 is for trees
burn_model <- C5.0(burn_train1, burn_test2_isdead1) 

burn_model_pred <- predict(burn_model, burn_test)

CrossTable(burn_test2_isdead1, burn_model_pred, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)

#bosting with more trials
burn_model50 <- C5.0(burn_train1, burn_test2_isdead1, trials = 50) 

burn_model_pred50 <- predict(burn_model50, burn_test)

CrossTable(burn_test2_isdead1, burn_model_pred50, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)

#trying tune model by adding weights

matrix_dimensions <- list(c("0", "1"), c("0", "1")) 
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions) 
error_cost

burn_cost <- C5.0(burn_train1, burn_test2_isdead1, costs = error_cost) 

burn_cost_pred <- predict(burn_cost, burn_test)

CrossTable(burn_test2_isdead1, burn_cost_pred, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)



predicted <- as.factor(dplyr::if_else(glm.prediction <= .85, 0, 1))
predicted

truth <- as.factor(burn_test$death)
# to tune logistic is to only change the predictors
### End Logistic regression


### CART Decision Tree
cartdata.hp <- rpart(death ~ tbsa + age + flame +inh_inj +race +gender, 
  data = burn_train)
cartdata.hp1 <- rpart(death ~ tbsa + age + flame +inh_inj +race +gender, 
                     data = burn1000)

rpart.plot(cartdata.hp, box.palette = c("Reds"), 
           under.cex = 1.4, cex.main = 1.5,
           type = 4)

# Evaluate CART on test
burn_cart_pred <- predict(cartdata.hp, burn_test)
burn_cart_pred_f <- as.factor(dplyr::if_else(burn_cart_pred <= .85, 0, 1))

truth_f <- factor(burn_test2_isdead1)

caret::confusionMatrix(burn_cart_pred_f, 
                       truth_f, 
                       positive = "1")
# Kappa only 0.4054
# build cart on train, then predict this model on test, then confusion matric on model and test data

## End of decision tree


### RF
burn_train1_isdead1 <- factor(burn_train1_isdead)
burn_rf_model <- randomForest(burn_train1_isdead1 ~ 
                                tbsa + age + flame + inh_inj + race + gender, 
             data = burn_train)
# Evaluate RF on test
burn_rf_pred <- predict(burn_rf_model, burn_test)


truth_f <- factor(burn_test2_isdead1)

caret::confusionMatrix(burn_rf_pred , 
                       truth_f, 
                       positive = "1")
# Kappa 0.6746

## End of evaluation of RF
