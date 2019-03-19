## Import data 
## by setting Working directory
setwd("C:/Users/simatit/Desktop")
EEGData <- read.csv("EEG_data.csv", na.strings = c(""," ","NA"))     

############################## Missing Values #########################
## Visualize Na terms
library(Amelia)
missmap(EEGData)
sapply(EEGData,function(x) sum(is.na(x)))

#### Delete Obervations with NA values
compEEG <- na.omit(EEGData)

#### Impute mean/median/mode 
library(ggplot2)

#### EEG Data
ggplot(EEGData, aes(1, Attention)) + geom_boxplot()
hist(EEGData$Attention)
# Impute by Median
EEGData$Attention[is.na(EEGData$Attention)]<-
  median(EEGData$Attention, na.rm = T)

## Impute using package imputeMissings
library(imputeMissings)
l<-impute(EEGData, method = "median/mode")

## Mice Package
library(mice)
d<-EEGData[,c(2:15)]
imputed_Data <- mice(d, m=5, maxit = 50, method = 'pmm', seed = 12000)

############################# Outliers Treatment ###################
## Attention Variable
library(ggplot2)
ggplot(l, aes(1,Attention)) + geom_boxplot(outlier.colour = "red",
                                           outlier.shape = 2)
## Labeling Outliers 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

library(dplyr)
l %>%
  mutate(outlier = ifelse(is_outlier(Attention), Attention, as.numeric(NA))) %>%
  ggplot(.,aes(1,Attention)) + geom_boxplot(fill = "steelblue",outlier.colour = "red",
                                            outlier.shape = 2)+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

## Attention
boxplot(l$Attention)
ggplot(d, aes(1,Attention)) + geom_boxplot(outlier.colour = "red",outlier.shape = 2)
qnt <- quantile(l$Attention, 0.75, na.rm = T)
caps <- quantile(l$Attention, 0.95, na.rm = T)
H <- 1.5 * IQR(l$Attention, na.rm = T)
l$Attentionis[l$Attention > (qnt +  H)] <- caps

## relationship among various features
             
contVars<-c("SubjectID","VideoID","Mediation","Raw","Delta","Theta","Alpha1","Alpha2","Beta1","Beta2","Gamma1","Gamma2","predefinedlabel","user-definedlabeln")
cont_df<-d[,names(d) %in% contVars]
## Scatter plot
pairs(cont_df)
library(corrplot)
corrplot(cor(cont_df), type = "full", "ellipse")

# 
ggplot(l, aes(SubjectID , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(VideoID , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Mediation , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Raw , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Delta , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Theta , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Alpha1 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Alpha2 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Beta1 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Beta2 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Gamma1 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Gamma2 , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(predefinedlabel , Attention)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(user-definedlabeln , Attention)) + geom_boxplot(fill = "steelblue")

### Data Modelling

# creating train and test data
str(EEGData)
train_proj <- EEGData[1:10249,] # creating train data
test_proj <- EEGData[10250:12812,]# creating test data

# removing Loan ID from  train and test data
train_proj <- subset(train_proj,select = -c(SubjectID))
test_proj <- subset(test_proj,select = -c(SubjectID))

## Logistic Regression
str(train_proj)
logistic<- glm(Attention~VideoID+Mediation+Raw+Delta+Theta+Alpha1+Alpha2+Beta1+Beta2+Gamma1+Gamma2+predefinedlabel+user.definedlabeln, data = train_proj)
summary(logistic)

logistic1<- glm(Attention~VideoID+Mediation+Raw+Delta+Theta+Alpha1+Alpha2+Beta1+Beta2+Gamma1+Gamma2+predefinedlabel+user.definedlabeln, data = test_proj)
summary(logistic1)

# prediction #   
predict<- predict(logistic, type = "response")
head(predict, 3)
train_proj$predict<- predict
train_projRound<- round(predict, digits = 0)
table(train_proj$Mediation, predict >= 0.5)

predict1<- predict(logistic1, type = "response")
head(predict1, 3)
test_proj$predict1<- predict1
test_projRound<- round(predict1, digits = 0)
table(test_proj$Mediation, predict1 <= 0.5)

confusionMatrix<- confusionMatrix(predictions,test_proj$Mediation)
confusionMatrix(glm.predict, train_proj$Mediation, positive = "Yes")

confusionMatrix

#ROC curve 
library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(Mediation$predict, train_proj$diagnosis)
pr <- train_proj$predict

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#............Random forest.........

set.seed(1)
library(randomForest)
a.EEGData<-randomForest(Attention~.,EEGData,
                            subset = train_proj,mtry = 3,importance = TRUE)
dim(a.EEGData)
importance(a.EEGData)

varImpPlot(a.EEGData,col = 'blue',pch = 10, cex = 1.25)

a.EEGData

test.pred.rf<-predict(a.EEGData, newdata = EEGData[-train_proj,],type = 'class')
table(test.pred.rf,test_proj)

