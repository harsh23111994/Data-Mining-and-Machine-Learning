#importing required libraries
library(tidyverse) #easy data manipulation and visualization
library(caret) #easy machine learning workflow
library(leaps) #computing stepwise regression
library(magrittr)
library(corrplot)
library(caTools)
library(pROC)
library(ggthemes)
library(cowplot)
library(dplyr)

#Custom Function for evaluating models
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
setwd("C:/Users/Harsh/Desktop/NCI/Data Mining and ML/Project/Data Set")
# Loading The Dataset -----------------------------------------------------
##############################Data Set 2####################################
rain <- read.csv("WeatherAUS.csv",na.strings=c("","NA"),header = T)
dim(rain)#check dimension of data
# Understanding Data ------------------------------------------------------
#Check structure and look for NAs
str(rain)
dplyr::glimpse(rain)

#Removing columns which are not important for analysis
drop = c("RISK_MM","WindGustDir","WindDir9am","WindDir3pm","RainToday","Date","Location")
rain = rain[,!(names(rain) %in% drop)]
str(rain)

#count of NA Values
library(Amelia)
library(mlbench)
library(Rcpp)
missmap(rain, col=c("blue", "red"), legend=FALSE)
sapply(rain, function(x) sum(is.na(x)))
#checking Distribution of variables and imputing median values
hist(rain$Sunshine)
exclude = c("RainTomorrow")
value = rain[,!(names(rain) %in% exclude)]
library(dplyr)
value = value %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
#checking NA values after cleaning
sapply(value, function(x) sum(is.na(x)))
summary(value)
str(value)

#str(rain$RainTomorrow)
#rain$RainTomorrow <- factor(rain$RainTomorrow,labels=c(0,1), levels=c("Yes","No"))
value$RainTomorrow = rain$RainTomorrow


value$RainTomorrow = ifelse(value$RainTomorrow == "Yes",1,0)
str(value)
#value$RainTomorrow =  as.numeric(value$RainTomorrow)
sapply(value, function(x) sum(is.na(x)))#checking NA values after cleaning

######################## Correlation ###############################################
#Building correlation plot among variables
numeric <- names(select_if(value, is.numeric))
cor_df <- as.matrix(value[numeric])
cor_mat <- cor(as.matrix(cor_df))

cor_mat
#Visualising the correlation matrix
corrplot(cor_mat, method = "pie",type = "lower", order = "FPC", tl.col = "black",sig.level=0.05 ,tl.srt = 45)

#Create Sample Test and Train dataset
library(caret)
index <- createDataPartition(value$RainTomorrow, p = 0.75, list = FALSE)
#index <- sample(1:dim(rain)[1], dim(rain)[1] * .75, replace=FALSE)
#index = sample.int(n = nrow(rain), size = floor(.75*nrow(rain)), replace = F)
AUStrain  <- value[index,]
AUStest <- value[-index,]

#Training on different models-----------------------------------------------

# Logisitic Regression ----------------------------------------------------
model <- glm(RainTomorrow ~ ., data = AUStrain ,family="binomial"(link='logit'))
summary(model)
fwd_model <- step(glm(RainTomorrow ~ ., data = AUStrain ,family="binomial"(link='logit')),direction = "forward")
summary(fwd_model)
bk_model <- step(glm(RainTomorrow ~ ., data = AUStrain ,family="binomial"(link='logit')),direction = "backward")
summary(bk_model)
#The above predict function does not give strictly 2 levels of value but rather gives
#probablistic (exponential) value between 0 and 1. So,if prob >0.5 value is 1 else 0
#bcz hypothesis function of logistic regresssion is sigmoid function.
Result = as.factor(ifelse(predict(bk_model,newdata = AUStest) >1,1,0))

view(Result)
# Result = ifelse(Result==1,0,1)
# Result = as.factor(as.numeric(Result))
AUStest$RainTomorrow = as.factor(AUStest$RainTomorrow)

confusionMatrix(Result,AUStest$RainTomorrow)
roc(as.integer(Result),as.integer(AUStest$RainTomorrow))
library(ROCR)

#KNN----------------------------------------------------------------------
# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
#First Normalize the dataset
Norm_KNN <- as.data.frame(lapply(value[,-17], normalize))
Norm_KNN$rain = value$RainTomorrow
summary(Norm_KNN)

target_category <- AUStrain[,17]
test_category = AUStrain[,17]

library(class)
##run knn function
knn_model <- knn(AUStrain,AUStest,cl=target_category,k=10)
confusionMatrix(Result,AUStest$RainTomorrow)
knn.roc = plot(roc(AUStest$RainTomorrow, as.integer(knn_model), direction="<"),
               col="red", lwd=3, main="ROC Curve for SVM")
roc(as.integer(knn_model),as.integer(AUStest$RainTomorrow))
##create confusion matrix
tab <- table(knn_model,AUStest$RainTomorrow)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#SVM ---------------------------------------------------------------------------
#Predicting
library(e1071)
svm_classifier <- svm(RainTomorrow ~., data=AUStrain,scale = 1)
svm_classifier

svm_pred = predict(svm_classifier,test_set)
confusionMatrix(svm_pred, test_set$STATISTICAL_MURDER_FLAG, dnn = c("Prediction", "Reference"))

table(svm_pred,test$STATISTICAL_MURDER_FLAG)

#Extra Grapgh---------------------------------------------------------------------------
df <- df %>% filter(Year>2007) %>% group_by(Year) %>% summarise( Temp = mean(Humidity))
qplot(data=df,Year,Temp,main="India Average Temperature 1900-2013", geom=c("point","smooth")) + aes(colour = Temp)+
  scale_color_gradient(low="purple", high="orange") +ylab("Temperature (°C)") +
  theme(legend.position = "bottom",axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#EFF2F4"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size=18,face = "bold"))
#--------------------------------------Plot Combined ROC ----------------------------------
plot(rf.roc,col="darkred")
plot(svm.roc,col="darkgreen",add=TRUE)
plot(lda.roc,col="blue",add=TRUE)
plot(dt.roc,col="orange",add=TRUE)
plot(knn.roc,col="black",add=TRUE)
legend(0.4,0.60, c('svm','decision tree','KNN','rf','lda'),cex=0.7,lty=c(6,1),bty = 5,
       lwd=c(4,1),col=c('darkred','darkgreen','blue','orange','black'))

