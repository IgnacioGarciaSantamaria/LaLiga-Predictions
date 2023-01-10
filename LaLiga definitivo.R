df_0910 <- read.csv("season-0910_csv.csv", stringsAsFactors = FALSE)
df_1011 <- read.csv("season-1011_csv.csv", stringsAsFactors = FALSE)
df_1112 <- read.csv("season-1112_csv.csv", stringsAsFactors = FALSE)
df_1213 <- read.csv("season-1213_csv.csv", stringsAsFactors = FALSE)
df_1314 <- read.csv("season-1314_csv.csv", stringsAsFactors = FALSE)
df_1415 <- read.csv("season-1415_csv.csv", stringsAsFactors = FALSE)
df_1516 <- read.csv("season-1516_csv.csv", stringsAsFactors = FALSE)
df_1617 <- read.csv("season-1617_csv.csv", stringsAsFactors = FALSE)
df_1718 <- read.csv("season-1718_csv.csv", stringsAsFactors = FALSE)
df_1819 <- read.csv("season-1819_csv.csv", stringsAsFactors = FALSE)

col_needed <- c("HomeTeam", "AwayTeam", "Date",
                "FTHG", "FTAG", "FTR",
                "HTHG", "HTAG", "HTR",
                "HS", "AS", "HST", "AST",
                "HF", "AF", "HC", "AC",
                "HY", "AY", "HR", "AR" ,
                "B365H", "B365D", "B365A",  ## bet365
                "BWH", "BWD", "BWA",        ## bet and win
                "WHH", "WHD", "WHA",        ## William Hill
                "VCH", "VCD", "VCA")        ## BetVictor

df_0910 <- data.frame(Season="2009-2010" , df_0910[,col_needed])
df_1011 <- data.frame(Season="2010-2011" , df_1011[,col_needed])
df_1112 <- data.frame(Season="2011-2012" , df_1112[,col_needed])
df_1213 <- data.frame(Season="2012-2013" , df_1213[,col_needed])
df_1314 <- data.frame(Season="2013-2014" , df_1314[,col_needed])
df_1415 <- data.frame(Season="2014-2015" , df_1415[,col_needed])
df_1516 <- data.frame(Season="2015-2016" , df_1516[,col_needed])
df_1617 <- data.frame(Season="2016-2017" , df_1617[,col_needed])
df_1718 <- data.frame(Season="2017-2018" , df_1718[,col_needed])
df_1819 <- data.frame(Season="2018-2019" , df_1819[,col_needed])

df_0910$Date <- as.Date.character(df_0910$Date)
df_1011$Date <- as.Date.character(df_1011$Date, "%d/%m/%y")
df_1112$Date <- as.Date.character(df_1112$Date, "%d/%m/%y")
df_1213$Date <- as.Date.character(df_1213$Date, "%d/%m/%y")
df_1314$Date <- as.Date.character(df_1314$Date, "%d/%m/%y")
df_1415$Date <- as.Date.character(df_1415$Date, "%d/%m/%y")
df_1516$Date <- as.Date.character(df_1516$Date, "%d/%m/%y")
df_1617$Date <- as.Date.character(df_1617$Date, "%d/%m/%y")
df_1718$Date <- as.Date.character(df_1718$Date, "%d/%m/%y")
df_1819$Date <- as.Date.character(df_1819$Date, "%d/%m/%Y")



## Combine Datasets

df_raw <- rbind(df_0910,df_1011, df_1112, df_1213, df_1314,
                df_1415, df_1516, df_1617, df_1718, df_1819)

rm(df_0910,df_1011, df_1112, df_1213, df_1314,
   df_1415, df_1516, df_1617, df_1718, df_1819)


summary(is.na(df_raw))

df_raw <- na.omit(df_raw)

get_last_3 <- function(){
  for (i in 1:nrow(df_raw)) {
    # saco los puntos de cada equipo en cada jornada
    df_raw[i,"HP" ] <- ifelse(df_raw[i,"FTR" ]=="H",3,ifelse(df_raw[i,"FTR" ]=="A",0,1))
    df_raw[i,"AP" ] <- ifelse(df_raw[i,"FTR" ]=="A",3, ifelse(df_raw[i,"FTR" ]=="H",0,1))
    
    date <- df_raw[(df_raw$Date < df_raw[i,"Date"]),]
    home <- date[(date$HomeTeam==df_raw[i,"HomeTeam"]),]
    away <- date[(date$AwayTeam==df_raw[i,"AwayTeam"]),]
    
    
    
    # solo quiero los tres ultimos partidos
    if (nrow(home)>3){
      home <- tail(home, 3)
    }
    if (nrow(away)>3){
      away <- tail(away, 3)
    }
    
    # para los primeros partidos ( no hay suficientes para la media)
    if (dim(home)[1] == 0) {
      home <- df_raw[i,]
    }
    if (dim(away)[1] == 0) {
      away <- df_raw[i,]
    }
    
    
    df_raw[i,"FTHG_avg" ] <- mean(home[,"FTHG"])
    df_raw[i,"FTAG_avg" ] <- mean(away[,"FTAG"])
    
    df_raw[i,"HTHG_avg" ] <- mean(home[,"HTHG"])
    df_raw[i,"HTAG_avg" ] <- mean(away[,"HTAG"])
    
    df_raw[i,"HS_avg" ] <- mean(home[,"HS"])
    df_raw[i,"AS_avg" ] <- mean(away[,"AS"])
    
    df_raw[i,"HST_avg" ] <- mean(home[,"HST"])
    df_raw[i,"AST_avg" ] <- mean(away[,"AST"])
    
    df_raw[i,"HF_avg" ] <- mean(home[,"HF"])
    df_raw[i,"AF_avg" ] <- mean(away[,"AF"])
    
    df_raw[i,"HC_avg" ] <- mean(home[,"HC"])
    df_raw[i,"AC_avg" ] <- mean(away[,"AC"])
    
    df_raw[i,"HY_avg" ] <- mean(home[,"HY"])
    df_raw[i,"AY_avg" ] <- mean(away[,"AY"])
    
    df_raw[i,"HR_avg" ] <- mean(home[,"HR"])
    df_raw[i,"AR_avg" ] <- mean(away[,"AR"])
    
    df_raw[i,"HP_avg" ] <- mean(home[,"HP"])
    df_raw[i,"AP_avg" ] <- mean(away[,"AP"])
    
    
    
  }
  return(df_raw)
  
}

liga <- get_last_3()
liga_avg <- liga

liga <- liga_avg[,c("Season","Date","HomeTeam","AwayTeam","FTHG_avg","FTAG_avg","HS_avg","AS_avg","HST_avg","AST_avg",
                    "HC_avg","AC_avg","HF_avg","AF_avg","HY_avg","AY_avg","HR_avg","AR_avg","HP_avg","AP_avg","B365A","B365H","B365D")]
liga$HomeTeam <- as.factor(liga$HomeTeam)
liga$AwayTeam <- as.factor(liga$AwayTeam)
liga$Season <- as.factor(liga$Season)


index <- liga$Season %in% "2018-2019"
test <- liga[index,]
train <- liga[-index,]


#####Regresión lineal
library(tidyverse)      #data manipulation and visualization
library("car")  # For VIF
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("plot3D")
library("modelr")  # For add_predictions
library("ISLR")


model1_visitante <- lm(FTAG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
                         HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D,data=train)
summary(model1_visitante)
confint(model1_visitante) #intervalos de confianza

model1_local <- lm(FTHG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
              HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D,data=train)
summary(model1_local)
confint(model1_local) #intervalos de confianza



model2_visitante <- lm(FTAG_avg ~ HomeTeam+AwayTeam+AS_avg+AST_avg+
                         AC_avg+AP_avg+B365A+B365H+B365D,data=train)
summary(model2_visitante)
confint(model2_visitante) #intervalos de confianza
vif(model2_visitante)


model2_local <- lm(FTHG_avg ~ HomeTeam+AwayTeam+HS_avg+
                     HC_avg+HP_avg+B365H+B365D,data=train)
summary(model2_local)
confint(model2_local) #intervalos de confianza
vif(model2_local)



model3_visitante <- lm(FTAG_avg ~ HomeTeam+AwayTeam+AS_avg+AST_avg+
                         AC_avg+AP_avg+B365A+AS_avg*AC_avg+AS_avg*AP_avg,data=train)
summary(model3_visitante)
confint(model3_visitante) #intervalos de confianza
vif(model3_visitante)

model3_local <- lm(FTHG_avg ~ HomeTeam+AwayTeam+HS_avg+
                     HC_avg+HP_avg+B365H+B365D+HS_avg*HC_avg+HP_avg*HS_avg,data=train)
summary(model3_local)
confint(model3_local) #intervalos de confianza
vif(model3_local)


###########################################3
predict.train.rlm.local <- predict(model3_visitante, newdata = train)
predict.test.rlm.local <- predict(model3_visitante, newdata = test)

MSE.lr.train <- mean((train$FTAG_avg - predict.train.rlm.local)^2)
MSE.lr.test  <- mean((test$FTAG_avg - predict.test.rlm.local)^2)

plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.rlm.local, type = "p", col = "blue")


plot(predict.test.rlm.local)
error_test<-test$FTAG_avg - predict.test.rlm.local

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.rlm.local,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


#################################################3
predict.train.rlm.local <- predict(model3_local, newdata = train)
predict.test.rlm.local <- predict(model3_local, newdata = test)

MSE.lr.train <- mean((train$FTHG_avg - predict.train.rlm.local)^2)
MSE.lr.test  <- mean((test$FTHG_avg - predict.test.rlm.local)^2)

plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.rlm.local, type = "p", col = "blue")


plot(predict.test.rlm.local)
error_test<-test$FTAG_avg - predict.test.rlm.local

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.rlm.local,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)



################################# LASSO-RIDGE
library(glmnet)
library(tidyverse)
library("ISLR")
library(caret)   # for model regression

#CASO 2  LOCAL
glmnet_grid_2 <- expand.grid(alpha = c(0, .1,  .2, .3, .4, .5, .6,.7 ,.8, .9, 1),
                             lambda = seq(0.01, 200, length = 40))

# CV method using 10 folds
glmnet_ctrl_2 <- trainControl(method = "cv", number = 10)
glmnet_fit_2 <- train(FTHG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
                        HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      tuneGrid = glmnet_grid_2,
                      trControl = glmnet_ctrl_2)
# Results obtained
glmnet_fit_2

# best result
glmnet_fit_2$bestTune


# plotting results
trellis.par.set(caretTheme())
plot(glmnet_fit_2, scales = list(x = list(log = 2)))
glmnet_grid_best <- expand.grid(alpha = 0.6,lambda = 0.01)




# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_best <- train(FTHG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
                       HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneGrid = glmnet_grid_best,
                     trControl = glmnet_ctrl)

predict.train.lr <- predict(glmnet_best, newdata = train)
predict.test.lr <- predict(glmnet_best, newdata = test)

MSE.lr.train <- mean((train$FTHG_avg - predict.train.lr)^2)
MSE.lr.test  <- mean((test$FTHG_avg - predict.test.lr)^2)

plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.lr, type = "p", col = "blue")


plot(predict.test.lr)
error_test<-test$FTAG_avg - predict.test.rlm.local

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.lr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
                     
                     


#CASO 2 visitante
glmnet_grid_2 <- expand.grid(alpha = c(0, .1,  .2, .3, .4, .5, .6,.7 ,.8, .9, 1),
                             lambda = seq(0.01, 200, length = 40))

# CV method using 10 folds
glmnet_ctrl_2 <- trainControl(method = "cv", number = 10)
glmnet_fit_2 <- train(FTAG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
                        HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      tuneGrid = glmnet_grid_2,
                      trControl = glmnet_ctrl_2)
# Results obtained
glmnet_fit_2

# best result
glmnet_fit_2$bestTune


# plotting results
trellis.par.set(caretTheme())
plot(glmnet_fit_2, scales = list(x = list(log = 2)))
glmnet_grid_best <- expand.grid(alpha = 0.5,lambda = 0.01)




# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_best <- train(FTAG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                       HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneGrid = glmnet_grid_best,
                     trControl = glmnet_ctrl)

predict.train.lr <- predict(glmnet_best, newdata = train)
predict.test.lr <- predict(glmnet_best, newdata = test)

MSE.lr.train <- mean((train$FTAG_avg - predict.train.lr)^2)
MSE.lr.test  <- mean((test$FTAG_avg - predict.test.lr)^2)

plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.lr, type = "p", col = "blue")


plot(predict.test.lr)
error_test<-test$FTAG_avg - predict.test.rlm.local

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.lr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)









#####################Red neuronal
library(glmnet)
library(tidyverse)
library("ISLR")
library(caret)   # for model regression
library(neuralnet)  # for MLP training
library(rsample)    # for data splitting
library(RSNNS)      # for MLP training

maxs <- apply(liga[,5:23], 2, max) 
mins <- apply(liga[,5:23], 2, min)

inputs<-liga[,8:23]
targets<-liga[,5:6]

#Nomalization
inputs_norm<-normalizeData(inputs, type="0_1")
target_norm<-normalizeData(targets, type="0_1")

sets<-splitForTrainingAndTest(inputs_norm,target_norm,ratio=0.3)

# NN training
mlp_model<-mlp(sets$inputsTrain, sets$targetsTrain, size=c(5,3),
               initFunc="Randomize_Weights",
               initFuncParams=c(-0.3, 0.3),
               learnFunc="Std_Backpropagation",
               learnFuncParams=c(0.2, 0.0),
               maxit = 350,
               updateFunc="Topological_Order",
               hiddenActFunc="Act_Logistic",
               linOut=TRUE,
               inputsTest = sets$inputsTest, 
               targetsTest = sets$targetsTest)

pred_ts_norm <- predict(mlp_model, sets$inputsTest)
#Plotting error
plotIterativeError(mlp_model)

set.seed(3141592) 

# creation of training and test datasets
normalization <- function(x) {return ((x-min(x))/(max(x)-min(x)))}
train_norm <- as.data.frame(lapply(train[,5:23], normalization))
test_norm <- as.data.frame(lapply(test[,5:23], normalization))


#training the neural network
set.seed(3141592)
nn <- neuralnet(FTHG_avg+FTAG_avg~.,
                       data=train_norm[1:1000,], hidden=c(3), 
                       lifesign = "minimal", 
                       linear.output = FALSE, rep =30)
plot(nn, rep="best")

best_rep<-which.min(nn$result.matrix[1,])

nn$weights[best_rep]

# activation function
nn$act.fct


#prediction with training set
pr.nn_training <- predict(nn, train_norm, rep=best_rep,all.units=FALSE)
pr.nn_test <- predict(nn, test_norm, rep=best_rep,all.units=FALSE)
# Descaling for comparison
predict.train.nn.local <- pr.nn_training[,1]*(maxs[1]-mins[1])+mins[1]
predict.train.nn.visitante <- pr.nn_training[,2]*(maxs[2]-mins[2])+mins[2]
predict.test.nn.local <- pr.nn_test[,1]*(maxs[1]-mins[1])+mins[1]
predict.test.nn.visitante <- pr.nn_test[,2]*(maxs[2]-mins[2])+mins[2]

MSE.nn.test.local <- mean((test$FTHG_avg - predict.test.nn.local)^2)
MSE.nn.train.local <- mean((train$FTHG_avg - predict.train.nn.local)^2)

MSE.nn.test.visitante <- mean((test$FTAG_avg - predict.test.nn.visitante)^2)
MSE.nn.train.visitante <- mean((train$FTAG_avg - predict.train.nn.visitante)^2)



####AWAY
plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.nn.visitante, type = "p", col = "blue")


plot(predict.test.lr)
error_test<-test$FTAG_avg - predict.test.nn.visitante

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.nn.visitante,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


####Local
plot(test$FTHG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.nn.local, type = "p", col = "blue")


plot(predict.test.lr)
error_test<-test$FTHG_avg - predict.test.nn.local

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.2, col=c('blue'))


# Compare predictions vs real values
plot(test$FTHG_avg,predict.test.nn.local,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


###############SVR AWAY

library(ggplot2) # Required for plotting with ggplot
library(e1071) #Required for SVM
library(vcd) # For mosaic
library(datasets) # Required for loading iris data

set.seed(1)
svr_train<-tune("svm",FTAG_avg ~ HomeTeam+AwayTeam+HTR+HS_avg+AS_avg+HST_avg+AST_avg+
                  HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, 
                data=train[1:200,], 
                kernel="radial",
                ranges=list(
                  epsilon=seq(0,1,0.1),
                  cost=c(0.01, 0.1, 1, 5, 10, 20),
                  gamma=c(0.1, 0.5, 1, 2, 5, 10)))

ggplot(data=svr_train$performances, aes(x=cost, y=error, 
                                        color=factor(epsilon)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs  C & epsilon")+
  theme_bw()+theme(legend.position ="bottom")

summary(svr_train)

#Saving the best model found for C, epsilon & gamma
best_model <- svr_train$best.model


summary(best_model)


## Calculate parameters of the SVR Model
#Find value of W
W=t(best_model$coefs) %*% best_model$SV

print(W)

predict.train.svr <- predict(best_model,train)
MSE.svr.train <- mean((train$FTAG_avg - predict.train.svr)^2)

predict.test.svr <- predict(best_model,test)
MSE.svr.test <- mean((test$FTAG_avg - predict.test.svr)^2)

plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.svr, type = "p", col = "blue")

error_test<-test$FTAG_avg-predict.test.svr

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.05, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,predict.test.svr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)



####################HOME TEAM

set.seed(1)
svr_train<-tune("svm",FTHG_avg ~ HomeTeam+AwayTeam+HS_avg+
                  HC_avg+HP_avg+B365H+B365D, 
                data=train[1:200,], 
                kernel="radial",
                ranges=list(
                  epsilon=seq(0,1,0.1),
                  cost=c(0.01, 0.1, 1, 5, 10, 20),
                  gamma=c(0.1, 0.5, 1, 2, 5, 10)))

ggplot(data=svr_train$performances, aes(x=cost, y=error, 
                                        color=factor(epsilon)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs  C & epsilon")+
  theme_bw()+theme(legend.position ="bottom")

summary(svr_train)

#Saving the best model found for C, epsilon & gamma
best_model <- svr_train$best.model


summary(best_model)


## Calculate parameters of the SVR Model
#Find value of W
W=t(best_model$coefs) %*% best_model$SV

print(W)

predict.train.svr <- predict(best_model,train)
MSE.svr.train <- mean((train$FTHG_avg - predict.train.svr)^2)

predict.test.svr <- predict(best_model,test)
MSE.svr.test <- mean((test$FTHG_avg - predict.test.svr)^2)

plot(test$FTHG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predict.test.svr, type = "p", col = "blue")

error_test<-test$FTHG_avg-predict.test.svr

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.05, col=c('blue'))


# Compare predictions vs real values
plot(test$FTHG_avg,predict.test.svr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)






#################################################ARBOL DE REGRESION AWAY
library(MASS)        # for obtaining data
library(tidyverse)  # for data processing
library(rpart)      # for CART decision tree
library(rpart.plot) # for plotting CART
library(caret)      # for confusion matrix and more
library(rsample)    # for data splitting

tree_result<- rpart(formula=FTAG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                      HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data=train, 
                    method='anova')

# Analysis of cp values in a table
printcp(tree_result, digits=6)

# Error evolution with increasing number of nodes
plotcp(tree_result, lty=2 , col="red", upper="size" )
plotcp(tree_result, lty=2 , col="red", upper="splits" )

best_cp<- tree_result$cptable[which.min(tree_result$cptable[,"xerror"]),"CP"]

tree_pruned<- prune(tree_result, cp=best_cp)
rpart.plot(tree_pruned, type=1, branch=0,tweak=1.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0)

pred_train <- predict(tree_pruned, newdata = train)
pred_test <- predict(tree_pruned, newdata = test)

MSE.rt.train <- mean((train$FTAG_avg - pred_train)^2)
MSE.rt.test <- mean((test$FTAG_avg - pred_test)^2)


plot(test$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(pred_test, type = "p", col = "blue")

error_test<-test$FTAG_avg-pred_test

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.25, col=c('blue'))


# Compare predictions vs real values
plot(test$FTAG_avg,pred_test,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)





##############################HOME
tree_result<- rpart(formula=FTHG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                      HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data=train, 
                    method='anova')

# Analysis of cp values in a table
printcp(tree_result, digits=6)

# Error evolution with increasing number of nodes
plotcp(tree_result, lty=2 , col="red", upper="size" )
plotcp(tree_result, lty=2 , col="red", upper="splits" )

best_cp<- tree_result$cptable[which.min(tree_result$cptable[,"xerror"]),"CP"]

tree_pruned<- prune(tree_result, cp=best_cp)
rpart.plot(tree_pruned, type=1, branch=0,tweak=1.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0)

pred_train <- predict(tree_pruned, newdata = train)
pred_test <- predict(tree_pruned, newdata = test)

MSE.rt.train <- mean((train$FTHG_avg - pred_train)^2)
MSE.rt.test <- mean((test$FTHG_avg - pred_test)^2)


plot(test$FTHG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(pred_test, type = "p", col = "blue")

error_test<-test$FTHG_avg-pred_test

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.25, col=c('blue'))


# Compare predictions vs real values
plot(test$FTHG_avg,pred_test,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)




#####################BOOSTING REGRESION AWAY
library(MASS)       # for obtaining data
library(tidyverse)  # for data processing
library(gbm)        # for Boosting algorithms

cv_error  <- vector("numeric")
n_trees <- vector("numeric")
shrinkage <- vector("numeric")


# Shrinkage optimization
for (i in c(0.001, 0.01, 0.1)) {
  set.seed(123)
  tree_boosting <- gbm(FTAG_avg ~ HS_avg+AS_avg+HST_avg+AST_avg+
                         HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                       distribution = "gaussian",
                       n.trees = 20000,
                       interaction.depth = 1,
                       shrinkage = i,
                       n.minobsinnode = 10,
                       bag.fraction = 0.5,
                       cv.folds = 5)
  cv_error  <- c(cv_error, tree_boosting$cv.error)
  n_trees <- c(n_trees, seq_along(tree_boosting$cv.error))
  shrinkage <- c(shrinkage, rep(i, length(tree_boosting$cv.error)))
}
error <- data.frame(cv_error, n_trees, shrinkage)

ggplot(data = error, aes(x = n_trees, y = cv_error,
                         color = as.factor(shrinkage))) +
  geom_smooth() +
  labs(title = "Evolution of the cv-error", color = "shrinkage") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Selecting tree complexity
cv_error  <- vector("numeric")
n_trees <- vector("numeric")
interaction.depth <- vector("numeric")
for (i in c(3, 4, 5, 8)) {
  set.seed(123)
  tree_boosting <- gbm(FTAG_avg ~ HS_avg+AS_avg+HST_avg+AST_avg+
                         HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                       distribution = "gaussian",
                       n.trees = 5000,
                       interaction.depth = i,
                       shrinkage = 0.01,
                       n.minobsinnode = 10,
                       bag.fraction = 0.5,
                       cv.folds = 5)
  
  cv_error  <- c(cv_error, tree_boosting$cv.error)
  n_trees <- c(n_trees, seq_along(tree_boosting$cv.error))
  interaction.depth <- c(interaction.depth,
                         rep(i, length(tree_boosting$cv.error)))
}
error <- data.frame(cv_error, n_trees, interaction.depth)

ggplot(data = error, aes(x = n_trees, y = cv_error,
                         color = as.factor(interaction.depth))) +
  geom_smooth() +
  labs(title = "Evolution of the cv-error", color = "interaction.depth") + 
  theme_bw() +
  theme(legend.position = "bottom")


#Selecting the minimum number of observations
cv_error  <- vector("numeric")
n_trees <- vector("numeric")
interaction.depth <- vector("numeric")
for (i in c(2, 3, 5, 10)) {
  tree_boosting <- gbm(FTAG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                         HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                       distribution = "gaussian",
                       n.trees = 5000,
                       interaction.depth = 5,
                       shrinkage = 0.01,
                       n.minobsinnode = i,
                       bag.fraction = 0.5,
                       cv.folds = 5)
  cv_error  <- c(cv_error, tree_boosting$cv.error)
  n_trees <- c(n_trees, seq_along(tree_boosting$cv.error))
  n.minobsinnode <- c(n.minobsinnode,
                      rep(i, length(tree_boosting$cv.error)))
}
error <- data.frame(cv_error, n_trees, n.minobsinnode)

ggplot(data = error, aes(x = n_trees, y = cv_error,
                         color = as.factor(n.minobsinnode))) +
  geom_smooth() +
  labs(title = "Evolution of the cv-error", color = "n.minobsinnode") + 
  theme_bw() +
  theme(legend.position = "bottom")



# Determination of the number of trees to use
set.seed(123)
tree_boosting <- gbm(FTAG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                       HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                     distribution = "gaussian",
                     n.trees = 500,
                     interaction.depth = 10,
                     shrinkage = 0.01,
                     n.minobsinnode = 2,
                     bag.fraction = 0.5,
                     cv.folds = 5)
error <- data.frame(cv_error = tree_boosting$cv.error,
                    n_trees = seq_along(tree_boosting$cv.error))
ggplot(data = error, aes(x = n_trees, y = cv_error)) +
  geom_line(color = "blue") +
  geom_point(data = error[which.min(error$cv_error),], color = "red") +
  labs(title = "Evolution of the cv-error") + 
  theme_bw() 

error[which.min(error$cv_error),]


set.seed(123)
tree_boosting_1 <- gbm(FTAG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                       HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                     distribution = "gaussian",
                     n.trees = 485,
                     interaction.depth = 4,
                     shrinkage = 0.01,
                     n.minobsinnode = 3,
                     bag.fraction = 0.5)

summary(tree_boosting)

importance_pred <- summary(tree_boosting, plotit = FALSE)
ggplot(data = importance_pred, aes(x = reorder(var, rel.inf), 
                                   y = rel.inf,
                                   fill = rel.inf)) +
  labs(x = "variable", title = "MSE reduction") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")


par(mfrow = c(1,2))
plot(tree_boosting, i.var = "AP_avg", col = "blue")
plot(tree_boosting, i.var = "AS_avg", col = "firebrick")


pred_train <- predict(tree_boosting_1, newdata = train)
pred_test <- predict(tree_boosting_1, newdata = test)
MSE.rt.train <- mean((train$FTAG_avg - pred_train)^2)
MSE.rt.test <- mean((test$FTAG_avg - pred_test)^2)


plot(train$FTAG_avg,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(pred_train, type = "p", col = "blue")

error_train<-train$FTAG_avg-pred_train

#Histogram of residuals
ggplot(data=as.data.frame(error_train), mapping= aes(x=error_train))+
  geom_histogram(binwidth=0.25, col=c('blue'))


# Compare predictions vs real values
plot(train$FTAG_avg,pred_train,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)




set.seed(123)
tree_boosting <- gbm(FTHG_avg ~ HomeTeam+AwayTeam+HS_avg+AS_avg+HST_avg+AST_avg+
                       HC_avg+AC_avg+HF_avg+AF_avg+HY_avg+AY_avg+HR_avg+AR_avg+HP_avg+AP_avg+B365A+B365H+B365D, data = train,
                     distribution = "gaussian",
                     n.trees = 485,
                     interaction.depth = 4,
                     shrinkage = 0.01,
                     n.minobsinnode = 3,
                     bag.fraction = 0.5)
pred_train_home <- predict(tree_boosting, newdata = train)
pred_test_home <- predict(tree_boosting, newdata = test)
MSE.home.train <- mean((train$FTAG_avg - pred_train)^2)
MSE.home.test <- mean((test$FTAG_avg - pred_test)^2)



ganador <- function (local, away){
  winner <- vector()
  for (i in 1:length(local)) {
    # saco los puntos de cada equipo en cada jornada
    winner[i] <- ifelse((local[i]-away[i])>0,"H",ifelse((local[i]-away[i])<0),"A","D")
    
    return (winner)
  }}

salida <- ganador(pred_test_home,pred_test)

prueba <- round(pred_test_home - pred_test)

hola <- vector()

for(i in 1:length(prueba)){
  if(prueba[i]>0){
    hola[i] <- "H"
  }
  if(prueba[i]<0){
    hola[i] <- "A"
  }
  else{
    hola[i] <- "D"
  }
}


for(i in 1:length(prueba)){
  hola[i] <- ifelse(prueba[i]>0,"H",ifelse(prueba[i]<0,"A","D"))
}


##########################
df_0910 <- read.csv("season-0910_csv.csv", stringsAsFactors = FALSE)
df_1011 <- read.csv("season-1011_csv.csv", stringsAsFactors = FALSE)
df_1112 <- read.csv("season-1112_csv.csv", stringsAsFactors = FALSE)
df_1213 <- read.csv("season-1213_csv.csv", stringsAsFactors = FALSE)
df_1314 <- read.csv("season-1314_csv.csv", stringsAsFactors = FALSE)
df_1415 <- read.csv("season-1415_csv.csv", stringsAsFactors = FALSE)
df_1516 <- read.csv("season-1516_csv.csv", stringsAsFactors = FALSE)
df_1617 <- read.csv("season-1617_csv.csv", stringsAsFactors = FALSE)
df_1718 <- read.csv("season-1718_csv.csv", stringsAsFactors = FALSE)
df_1819 <- read.csv("season-1819_csv.csv", stringsAsFactors = FALSE)

col_needed <- c("HomeTeam", "AwayTeam", "Date",
                "FTHG", "FTAG", "FTR",
                "HTHG", "HTAG", "HTR",
                "HS", "AS", "HST", "AST",
                "HF", "AF", "HC", "AC",
                "HY", "AY", "HR", "AR" ,
                "B365H", "B365D", "B365A",  ## bet365
                "BWH", "BWD", "BWA",        ## bet and win
                "WHH", "WHD", "WHA",        ## William Hill
                "VCH", "VCD", "VCA")        ## BetVictor

df_0910 <- data.frame(Season="2009-2010" , df_0910[,col_needed])
df_1011 <- data.frame(Season="2010-2011" , df_1011[,col_needed])
df_1112 <- data.frame(Season="2011-2012" , df_1112[,col_needed])
df_1213 <- data.frame(Season="2012-2013" , df_1213[,col_needed])
df_1314 <- data.frame(Season="2013-2014" , df_1314[,col_needed])
df_1415 <- data.frame(Season="2014-2015" , df_1415[,col_needed])
df_1516 <- data.frame(Season="2015-2016" , df_1516[,col_needed])
df_1617 <- data.frame(Season="2016-2017" , df_1617[,col_needed])
df_1718 <- data.frame(Season="2017-2018" , df_1718[,col_needed])
df_1819 <- data.frame(Season="2018-2019" , df_1819[,col_needed])

df_0910$Date <- as.Date.character(df_0910$Date)
df_1011$Date <- as.Date.character(df_1011$Date, "%d/%m/%y")
df_1112$Date <- as.Date.character(df_1112$Date, "%d/%m/%y")
df_1213$Date <- as.Date.character(df_1213$Date, "%d/%m/%y")
df_1314$Date <- as.Date.character(df_1314$Date, "%d/%m/%y")
df_1415$Date <- as.Date.character(df_1415$Date, "%d/%m/%y")
df_1516$Date <- as.Date.character(df_1516$Date, "%d/%m/%y")
df_1617$Date <- as.Date.character(df_1617$Date, "%d/%m/%y")
df_1718$Date <- as.Date.character(df_1718$Date, "%d/%m/%y")
df_1819$Date <- as.Date.character(df_1819$Date, "%d/%m/%Y")



## Combine Datasets

df_raw <- rbind(df_0910,df_1011, df_1112, df_1213, df_1314,
                df_1415, df_1516, df_1617, df_1718, df_1819)

rm(df_0910,df_1011, df_1112, df_1213, df_1314,
   df_1415, df_1516, df_1617, df_1718, df_1819)


summary(is.na(df_raw))

df_raw <- na.omit(df_raw)

df_raw$Season <- as.factor(df_raw$Season)
df_raw$HomeTeam <- as.factor(df_raw$HomeTeam)
df_raw$AwayTeam <- as.factor(df_raw$AwayTeam)
df_raw$FTR <- as.factor(df_raw$FTR)
df_raw$HTR <- as.factor(df_raw$HTR)

liga_test <- subset(df_raw, Season=="2018-2019" )
liga_train <- subset(df_raw, Season !="2018-2019" )
library(tidyverse)
library(gclus)
library(ltm)
library(reshape2)
library(dplyr)
library(caret)
library(rsample) # for data splitting
#library(predictr)
library(doParallel)
library(corrplot)
library(class) 
library(rpart)
library(e1071)
library(randomForest)
library(gbm)
library(nnet)
library(MASS)
library(kernlab)
library(snowfall)
library(nnet) 
library(nnet)

table <- table(liga_test$FTR, hola)
accuracy <- round((sum(diag(table))/sum(table))*100,2)




set.seed(123)
data_split<- initial_split(liga, prop=0.999)
train<- training(data_split)
test<- testing(data_split)

test <- test[,c("FTHG_avg","HomeTeam","AwayTeam","HS_avg","AS_avg","HST_avg","AST_avg",
                  "HC_avg","AC_avg","HF_avg","AF_avg","HY_avg","AY_avg","HR_avg","AR_avg","HP_avg","AP_avg","B365A","B365H","B365D")]
caso_home <- predict(tree_boosting_1, newdata = test)               
salida_home <- round(caso_home)
salida_home

test <- test[,c("FTAG_avg","HomeTeam","AwayTeam","HS_avg","AS_avg","HST_avg","AST_avg",
                "HC_avg","AC_avg","HF_avg","AF_avg","HY_avg","AY_avg","HR_avg","AR_avg","HP_avg","AP_avg","B365A","B365H","B365D")]
caso_away <- predict(tree_boosting_1, newdata = test)               
salida_away <- round(caso_home)
salida_away
caso_away

