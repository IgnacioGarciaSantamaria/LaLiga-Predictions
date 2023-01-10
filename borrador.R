rm(list=ls())

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
library(devtools)
library(neuralnet)
library(nnet)
# Leemos los datos

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


########################
#    GET AVG DATA     #
#######################

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


summary(is.na(liga))

liga <- liga_avg[,c("Season","Date","HomeTeam","AwayTeam","FTHG_avg","FTAG_avg","FTR","HTHG_avg","HTAG_avg","HTR","HS_avg","AS_avg","HST_avg","AST_avg",
                    "HC_avg","AC_avg","HF_avg","AF_avg","HY_avg","AY_avg","HR_avg","AR_avg","HP_avg","AP_avg","B365A","B365H","B365D")]

## CORRELATION PLOT

liga_num <- liga[ , c(4,5,8,9,11:25)] 
liga.cor <- cor(liga_num, use="pairwise", method="pearson")
ord <- order(liga.cor[1,])
liga.cor <- liga.cor[ord, ord]
corrplot(liga.cor, mar=c(0,0,1,0))
liga.cor
rcor.test(liga_num, method = "pearson")

## HEAT MAP CORRELATION

title <- "correlation heat map" 
corp <- qplot(x=Var1, y=Var2, data=melt(cor(liga_num, use="p")), fill=value, geom="tile") +  
  scale_fill_gradient2(limits=c(-1, 1))
corp <- corp + theme(axis.title.x=element_blank(), axis.text.x=element_blank()  
                     , axis.ticks=element_blank())
corp <- corp + ggtitle(title)  
corp  

## SCATTER PLOT

dta.col <- dmat.color(df_raw) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
cpairs(liga_num, ord, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


## FURTHER EXPLORING THE DATA
ggplot(df_raw, aes(x=FTR)) + geom_histogram(binwidth=5,stat="count")

# explaining Betting variables are not linearly correlated as was assumed from correlation plot above.
ggplot(df_raw, aes(log(B365A),log(B365D), color=FTR)) + geom_point() + geom_smooth()  
ggplot(df_raw, aes(log(B365A),log(B365H), color=FTR)) + geom_point() + geom_smooth()  




p <- ggplot(df_raw, aes(HR, AR),colour = factor(FTR)) + geom_point()
# With one variable
p + facet_grid(. ~ FTR)


d4 <- as.Date(df_raw$Date, "%d/%m/%y")
d4 <- strftime(d4, "%Y-%m-%d")
df_raw$Date <- d4

# grafico de barras lo hago en excel
write.csv(df_raw, file="df_raw.csv")


## Convert to Factor
df_raw$Season <- as.factor(df_raw$Season)
df_raw$HomeTeam <- as.factor(df_raw$HomeTeam)
df_raw$AwayTeam <- as.factor(df_raw$AwayTeam)
df_raw$FTR <- as.factor(df_raw$FTR)
df_raw$HTR <- as.factor(df_raw$HTR)


#DATOS FINALES PARA EL MODELO
liga <- df_raw[,c("Season","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST",
                    "HC","AC","HF","AF","HY","AY","HR","AR","B365A","B365H","B365D")]

# Normalize data
pp <- preProcess(liga, method = c("center", "scale", "BoxCox"))
liga <- predict(pp, liga)
# me quito las columas de goles pues no quiero q sean determinantes en el modelo y las de apuestas(ver documento)
#now we can use betting data in two ways- we can either keep it and include in our model to estimate winning team once we know the betting estimates from the experts or we can exclude it from the current model and utilize it at the end to make comparison with our estimates for win, draw or loss.
liga$FTHG <- NULL
liga$FTAG <- NULL
liga$Date <- NULL
#liga$B365A <- NULL
#liga$B365H <- NULL
#liga$B365D <- NULL

# vamos a partir de la base de q no se como han quedado en el descanso
liga$HTR <- NULL

### dos formas 
### podemos predecir partidos aleatorios o los de la última temporada
# Vamos a predecir FTR (full-time result)
set.seed(123)
data_split<- initial_split(liga, prop=0.6)
train<- training(data_split)
test<- testing(data_split)
train <-train[-c(5,6)]
test <- test[-c(5,6)]
train$Season <- as.factor(train$Season)
train$HomeTeam <- as.factor(train$HomeTeam)
train$AwayTeam <- as.factor(train$AwayTeam)
train$FTR <- as.factor(train$FTR)
train$HTR <- as.factor(train$HTR)
#me quedo solo con las predicciones de bet365
train<- train[c(1:23)]
#quito la fecha pq está mal (siempre pone fechas entre 2009 y 2010)
train<- train[-c(4)]

test$Season <- as.factor(test$Season)
test$HomeTeam <- as.factor(test$HomeTeam)
test$AwayTeam <- as.factor(test$AwayTeam)
test$FTR <- as.factor(test$FTR)
test$HTR <- as.factor(test$HTR)
test <- test[c(1:23)]
test <- test[-c(4)]

##################################
#         Data Partition         #
##################################

########## para predecir la ultima temporada

liga_test <- subset(liga, Season=="2018-2019" )
liga_train <- subset(liga, Season !="2018-2019" )



# para algunos modelos solo voy a usar las variables numericas
liga_train_num <- liga_train[-c(1,2,3)]
liga_test_num <- test[-c(1,2,3)]



# comprueblo si estan bien las proporciones
table(liga_train$FTR)
table(liga_test$FTR)
table(liga_train$FTR)/nrow(liga_train)
table(liga_test$FTR)/nrow(liga_test)

## Pre-model building.
library(colorspace)
numeric <- liga_train[-c(1,4)]
categoric <- liga_train[ "HTR"]
target  <- liga_train["FTR"]
require(Hmisc, quietly=TRUE)

# Principal Components Analysis (on numerics only).
pc <- prcomp(na.omit(numeric), scale=TRUE, center=TRUE, tol=0)
# Show the output of the analysis.
pc
# Summarise the importance of the components found.
summary(pc)
#Summary shows first 11 principal components are able to explain about 94% of the variability in the dataset.


# Display a plot showing the relative importance of the components.
plot(pc, main="")
title(main="Principal Components Importance")
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.
biplot(pc, main="")
title(main="Principal Components")

##################################
#      Multinomial logistic      #
##################################

liga_mn <- multinom(FTR ~ FTHG_avg+FTAG_avg+HST_avg+AST_avg+HP_avg+AP_avg+B365A+B365H+B365D+HR_avg+AR_avg, data=liga_train)
summary(liga_mn)
coefs <- coef(liga_mn)
coefs

zvalues <- summary(liga_mn)$coefficients / summary(liga_mn)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2

# Predicting the values for train dataset
predictions_mn<- predict(liga_mn, newdata = liga_train, "class")
# Building classification table
tab <- table(liga_train$FTR, predictions_mn)
tab
accuracy <- round((sum(diag(tab))/sum(tab))*100,2)
accuracy


#test 
predictions_mn<- predict(liga_mn, newdata = liga_test, "class")
# Building classification table
tab <- table(liga_test$FTR, predictions_mn)
tab
accuracy <- round((sum(diag(tab))/sum(tab))*100,2)
accuracy


#This variation of alpha is called mixing percentage
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))

# CV method using 5 folds
glmnet_ctrl <- trainControl(method = "cv", number = 5)
glmnet_fit <- train(FTR ~ ., data = liga_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)

glmnet_fit

# best result
glmnet_fit$bestTune


# plotting results
trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x = list(log = 2)))


# Prediction
pred_classes <- predict(glmnet_fit, newdata = liga_test)
table(pred_classes)
confusionMatrix(liga_test$FTR, pred_classes)
##################################
#                KNN             #
##################################

#splo me puedo quedar con las variables continuas

train.labels <- liga_train_num$FTR
test.labels <- liga_test_num$FTR
train_knn <- liga_train_num[-3]
test_knn <- liga_test_num[-3]



#uso como k la raiz cuadrada del numero total de observaciones k= 61 aprox

knn.61 <-  knn(train=train_knn, test=test_knn, cl=train.labels, k=61)


# Find optimal k
i=1
suggested.k <- 61
k.choices=1
k.max_value = 0
k.optm = 0
for (i in 1:suggested.k){ 
  knn.mod <-  knn(train=train_knn, test=test_knn, cl=train.labels, k=i)
  k.choices[i] <- 100 * sum(test.labels == knn.mod)/NROW(test.labels)
  k=i  
  cat(k,'=',k.choices[i],'\n')
  if (k.max_value < k.choices[i]) {
    k.optm <- i
    k.max_value <- k.choices[i]
  }
}
plot(k.choices, type="b", xlab="K- Value",ylab="Accuracy level") 
k.optm
knn.optimal <-  knn(train=train_knn, test=test_knn, cl=train.labels, k=k.optm)
# Confusion matrix of optimal model 100% accuracy 
table <- table(knn.optimal, test.labels)
table
accuracy <- round((sum(diag(table))/sum(table))*100,2)
accuracy



#confusion matrix for model with k=61
table <-table(knn.61, test.labels)
table
accuracy <- round((sum(diag(table))/sum(table))*100,2)
accuracy

##################################
#            KMeans              #
##################################

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(DMwR)       # for unscale

#uso solo las variables numericas liga_num
liga_scale <- scale(liga_num)
tdata<-t(liga_scale)


# computing distance matrix between the rows of the data matrix
distance <- get_dist(tdata)
# Visualization of a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB",
                                    mid = "white", high = "#FC4E07"))

#Optimum number of clusters. Elbow method
fviz_nbclust(train_knn, kmeans, method = "wss", k.max = 20)

#Optimum number of clusters. Silhouette method
fviz_nbclust(train_knn, kmeans, method = "silhouette")

#GAP method
gap_stat <- clusGap(train_knn, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 25)
fviz_gap_stat(gap_stat)

##3 clusters one per home , draw and away
cluster3<-kmeans(train_knn,centers=3,nstart=20)

# clustering results
str(cluster3)
cluster3

#visualization  of clusters 
fviz_cluster(cluster3, data=train_knn,
             choose.vars = c("FTHG_avg", "FTAG_avg"),stand =
               FALSE, ellipse.type = "norm") + theme_bw() 


fviz_cluster(cluster3, data=train_knn,
             stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

prediction<- clusters(test_knn, cluster3[["centers"]])



table_pred<-table(test.labels, prediction)
table_pred
accuracy <- round((sum(diag(table_pred))/sum(table_pred))*100,2)
accuracy

##################################
#      Decision Tree             #
##################################


liga_dt <- rpart(FTR ~ .,
                 data=liga_train,
                 method="class",
                 parms=list(split="information"),
                 control=rpart.control(usesurrogate=0, 
                                       maxsurrogate=0))
liga_dt
print(liga_dt)
printcp(liga_dt)

library(rattle)
library(rpart.plot)
fancyRpartPlot(liga_dt, main="Decision Tree- FTR")


asRules(liga_dt)

# sacar error y una visualización del árbol


##################################
#              C4.5              #
##################################

library(C50)
tree_result <- C5.0(FTR  ~ ., data=liga_train, rules = FALSE,
                    control = C5.0Control(
                      noGlobalPruning = FALSE, # Pruning is in effect
                      CF= 0.2))  #Higher CF less prunning

# con prunning hay 32% de error
tree_result2 <- C5.0(FTR  ~ ., data=liga_train, rules = FALSE,
                    control = C5.0Control(
                      noGlobalPruning = FALSE, # Pruning is in effect
                      CF=0.1)) #Higher CF less prunning

tree_result

i=0.1
suggested.i <- 100
for (i in 1:suggested.k){ 
  tree_result <- C5.0(FTR  ~ ., data=liga_train, rules = TRUE,
                      control = C5.0Control(
                        noGlobalPruning = FALSE, # Pruning is in effect
                        CF= i/100))  #Higher CF less prunning
  predictions <- predict(tree_result, newdata = liga_test, type ="class")
  
  table<-table(prediction=predictions, real= liga_test$FTR)
  
  error_classification <- mean(predictions != liga_test$FTR)

  cat(i/100,'=',error_classification,'\n')

}
plot(k.choices, type="b", xlab="K- Value",ylab="Accuracy level") 
k.optm
knn.optimal <-  knn(train=train_knn, test=test_knn, cl=train.labels, k=k.optm)


summary(tree_result)
#PLotting the tree
plot(tree_result, trial=0, subtree=NULL)


summary(tree_result2)
#PLotting the tree
plot(tree_result, trial=0, subtree=NULL)

summary(tree_2)
tree_2

tree_result2<- rpart(formula=FTR  ~ ., data=liga_train, method='class')

##################################
#              Rpart             #
##################################
tree_result<- rpart(formula=FTR  ~ ., data=liga_train, method='class')
##################################
#      Random Forest             #
##################################

# Random Forest 
# The 'randomForest' package provides the 'randomForest' function.
# Build the Random Forest model.


liga_rf <- randomForest(formula = (FTR) ~ .,data = liga_train,ntree = 500, mtry = 3,
                        importance = TRUE,na.action=na.roughfix, replace = FALSE)
# usar caret para hallar los parámetros óptimos (de momento uso estos)

liga_rf
# List the importance of the variables.
rn <- round(importance(liga_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]
# Plot the relative importance of the variables.
varImpPlot(liga_rf, main="")
title(main="Variable Importance Random Forest")
# Plot the error rate against the number of trees.
plot(liga_rf, main="")
legend("topright", c("OOB", "A", "D", "H"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")

require(verification)
aucc <- roc.area(as.integer(as.factor(liga_train$FTR))-1,
                 rf_model$votes[,2])$A
roc.plot(as.integer(as.factor(liga_train$FTR))-1,
         rf_model$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest")


tuning_rf_mtry <- function(df, y, ntree = 500){
  # This function returns the out-of-bag-MSE of a RandomForest model
  # in function of the number of predictors evaluated
  
  
  # Arguments:
  #   df = data frame with predictors and variable to predict
  #   y  = name of the variable to predict
  #   ntree = number of trees created by the randomForest algorithm
  
  require(dplyr)
  max_predictors <- ncol(df) - 1
  n_predictors   <- rep(NA, max_predictors)
  oob_err_rate   <- rep(NA, max_predictors)
  for (i in 1:max_predictors) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    model_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictors[i] <- i
    oob_err_rate[i] <- tail(model_rf$err.rate[,1], n = 1)
  }
  results <- data_frame(n_predictors, oob_err_rate)
  return(results)
}

hiperparameter_mtry <-  tuning_rf_mtry(df = liga_train, y = "FTR")
hiperparameter_mtry %>% arrange(oob_err_rate)

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # This funstion returns the out-of-bag-MSE of a random forestmodel
  # in function of the minimum size of the terminal nodes (nodesize).
  
  
  # Arguments:
  #   df = data frame with predictors and variable to predict
  #   y  = name of the variable to predict
  #   size= evaluated sizes
  #   ntree = number of trees created by the randomForest algorithm
  
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  
  oob_err_rate <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    model_rf <- randomForest(formula = f, data = df, mtry = 3, ntree = ntree,
                             nodesize = i)
    oob_err_rate[i] <- tail(model_rf$err.rate[, 1],n = 1)
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

hiperparameter_nodesize <-  tuning_rf_nodesize(df = liga_train, y = "FTR",
                                               size = c(1:20))
hiperparameter_nodesize %>% arrange(oob_err_rate)

model_randomforest <- randomForest(FTR ~ ., data = liga_train,
                                   mtry = 3 , ntree = 500, nodesize = 12,
                                   importance = TRUE)
oob_error_rate <- data.frame(oob_error_rate = model_randomforest$err.rate[,1],
                             trees = seq_along(model_randomforest$err.rate[,1]))


ggplot(data = oob_error_rate, aes(x = trees, y = oob_error_rate )) +
  geom_line() +
  labs(title = "Evolutionof the out-of-bag-error vs trees number",
       x = "number of trees") +
  theme_bw()


# Creating the random forest model
rf_model<- randomForest(formula=FTR  ~ ., data=liga_train, 
                        mtry=3, ntree=300, nodesize=12,
                        importance=TRUE, norm.votes=TRUE)



#Result of random forest model
print(rf_model)

rf_model

##################################
#   Support Vector Machine       #
##################################

# Build a Support Vector Machine model.

liga_ksvm <- ksvm(FTR ~ .,
                  data=liga_train,
                  kernel="rbfdot",
                  prob.model=TRUE)


# Generate a textual view of the SVM model.
liga_ksvm

liga_ksmv_tune<-tune("svm", FTR~., 
                data=train, 
                kernel="radial",
                ranges=list(
                  cost=c(0.01, 0.1, 1, 5, 10, 20),
                  gamma=c(0.1, 0.5, 1, 2, 5, 10)),
                tunecontrol= tune.control(sampling="cross",cross=10))

summary(liga_ksmv_tune)


# Plotting error versus cost
ggplot(data=liga_ksmv_tune$performances, aes(x=cost, y=error, 
                                        color=factor(gamma)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs  C & gamma")+
  theme_bw()+theme(legend.position ="bottom")

#Saving the best model found for C & gamma
best_model <- liga_ksmv_tune$best.model
best_model


#Prediction
prediction <- predict(best_model, test)
table <- table(test$FTR, prediction)

table
accuracy <- round((sum(diag(table))/sum(table))*100,2)
accuracy

#Confussion in training
prediction_tr <- predict(best_model, train)
table_tr <- table(train$FTR, prediction_tr)

table_tr
accuracy <- round((sum(diag(table_tr))/sum(table_tr))*100,2)
accuracy

#Prediction for the generic model
prediction <- predict(liga_ksvm, liga_test)
table <- table(liga_test$FTR, prediction)

table
accuracy <- round((sum(diag(table))/sum(table))*100,2)
accuracy

#Confussion in training
prediction_tr <- predict(liga_ksvm, liga_train)
table_tr <- table(liga_train$FTR, prediction_tr)

table_tr
accuracy <- round((sum(diag(table_tr))/sum(table_tr))*100,2)
accuracy



##################################
#        Neural Network          #
##################################
#Creo las variables dummies

library(dummies)  #for creration of dummy variables
liga_train_dm <- dummy.data.frame(data=liga_train_num, names="FTR", sep="_")
liga_test_dm <- dummy.data.frame(data=liga_test_num, names="FTR", sep="_")
#
#ann_model <- neuralnet(FTR_A+FTR_D+FTR_H~HTHG+HTAG+HS+AS+HST+AST+HC+AC+HF+AF+HY+AY+HR+AR+B365A+B365H+B365D,
#                       data=liga_train_dm, hidden=c(5,5), 
#                      lifesign = "minimal", 
#                      linear.output = FALSE, rep =3,threshold = 0.05)
#
#plot(ann_model, rep="best")

##################
nn <- neuralnet(FTR ~ .,data=liga_train_num, hidden=c(5,5), 
                lifesign = "minimal", 
                linear.output = FALSE, rep =20,threshold=0.2)

plot(nn,rep="best")

#prediction with training set
pr.nn_tr <- predict(nn, liga_train_num[,2:18],
                    rep=3,all.units=FALSE)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))
pred_train<-max.col(pr.nn_tr_round)
real_train<-max.col(liga_train_dm[,1:3])
print(real_train)
print(pred_train)
caret::confusionMatrix(table(real_train,pred_train))

liga_train<- scale(liga_train_num)

model<-nnet(FTR~.,data=liga_train_num,size = 5,decay = 0.0001, abstol = 1.0e-4, reltol = 1.0e-8,maxit = 1000)

prediction<-predict(model,liga_train_num,type="class")

mtab<-table(liga_train_num$FTR,prediction)
confusionMatrix(mtab)
library(NeuralNetTools)
plotnet(model)


##################################
#        Test all models         #
##################################


 
#multinominal logistic regression
# Predicting the values for test dataset
ligamn_pr <- predict(liga_mn, newdata = liga_test, "class")
# Building classification table
tab <- table(liga_test$FTR,ligamn_pr)
tab
accuracy <- round((sum(diag(tab))/sum(tab))*100,2)
accuracy

#KNN 
#no hay test. el modelo se crea al introducir el conjunto de test
knn.optimal <-  knn(train=train_knn, test=test_knn, cl=train.labels, k=58)
cm <- table(knn.optimal, test.labels)
cm
accuracy <- round((sum(diag(cm))/sum(cm))*100,2)
accuracy

#KMeans
prediction<- clusters(test_knn, cluster3[["centers"]])
table_pred<-table(test.labels, prediction)
table_pred
accuracy <- round((sum(diag(table_pred))/sum(table_pred))*100,2)
accuracy


#decision tree
liga_pr <- predict(liga_dt, newdata=liga_test, type="class")
liga_pr

cm <- confusionMatrix(liga_pr, liga_test$FTR)
cm
#86% de accuracy 

overallcm <- cm$overall
overallcm

# C4.5
predictions <- predict(tree_result, newdata = liga_train, type ="class")

table<-table(prediction=predictions, real= liga_train$FTR)
table
error_classification <- mean(predictions != liga_test$FTR)
error_classification
accuracy <- round((sum(diag(table))/sum(table))*100,2)
accuracy


#Random Forest
ligarf_pr <- predict(rf_model, newdata=(liga_train))
ligarf_pr

rfcm <- confusionMatrix(ligarf_pr, liga_train$FTR)
rfcm
overallrfcm <- rfcm$overall
overallrfcm
#100% de accuracy

#Support Vector Machine
liga_kvsmpr <- predict(liga_ksvm, newdata=liga_test)
liga_kvsmpr
kvsmcm <- confusionMatrix(liga_kvsmpr, liga_test$FTR)
kvsmcm
overallkvsm <- kvsmcm$overall
overallkvsm


#Neural Network
#prediction with test dataset
pr.nn_tr <- predict(nn, liga_test_num[,2:18],
                    rep=3,all.units=FALSE)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))
pred_test<-max.col(pr.nn_tr_round)
real_test<-max.col(liga_test_dm[,1:3])
print(real_train)
print(pred_train)
caret::confusionMatrix(table(real_train,pred_train))

##################################
#        Cross-validation        #
##################################
#      Elijo el mejor modelo de  #
#      cada uno y los comparo    #
##################################

#Decision tree
fit1 <- train(FTR ~ ., data = liga_train, method = "rpart", tuneLength = 30, 
             trControl = trainControl(method = "cv", repeats = 1, number = 5))
fit1

trellis.par.set(caretTheme())
plot(fit1, metric= "Kappa")
plot(fit1, metric= "Accuracy")


#Random Forest
fit3 <- train(FTR ~ ., data = liga_train, method = "rf", importance=TRUE, 
              trControl=trainControl(method = "cv",  
                                     number = 5,
                                     repeats = 1, selectionFunction = "oneSE"),
              prox=TRUE, allowParallel=TRUE)
fit3

trellis.par.set(caretTheme())
plot(fit3, metric="Kappa")
plot(fit3, metric="Accuracy")


#support vector machines
fit5 <- train(FTR ~ ., data = liga_train, method = "svmLinear",  tuneLength = 30, trControl =trainControl(method = "cv", repeats = 1, number=5))
fit5

# Multinomial logistic
fit6 <- train(FTR ~ ., data = liga_train, method = "multinom", maxit=1000, tuneLength=1, trControl = trainControl(method = "cv", number=5,repeats=1, savePredictions=TRUE))
fit6
#Since models are fit on the same versions of the training data, it makes sense to make inferences on the differences between models. In this way we reduce the within-resample correlation that may exist. We can compute the differences as well, for t-test



##################################
#        Model Selection         #
##################################


ganador <- function (local, away){
  winner <- vector()
  for (i in 1:length(local)) {
    # saco los puntos de cada equipo en cada jornada
    winner[i] <- ifelse(local[i]-away[i]>0,"H",ifelse(local[i]-away[i]<0,"D","A"))
  
  return (winner)
  }}

###vector 2

set.seed(123)
data_split<- initial_split(liga, prop=0.999)
train<- training(data_split)
test<- testing(data_split)



