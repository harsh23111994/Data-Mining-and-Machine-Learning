# Custom Function that returns Root Mean Squared Error
rmse <- function(error)
{
  return (print(sqrt(mean(error^2))))

}

# Custom Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#set Working Directory
setwd("C:/Users/Harsh/Desktop/NCI/Data Mining and ML/Project/Data Set")
#Read Data
data<- read.csv("testset.csv",na.strings=c("","NA"),header = T)
dim(data)
#Check structure and look for NAs
str(data)

#Transforming the data Converting 0 and 1's to Yes and No
data$X_hum =  as.integer(data$X_hum)
data$X_fog <- factor(data$X_fog,labels=c("YES", "NO"), levels=c(0,1))
data$X_snow <- factor(data$X_snow,labels=c("YES", "NO"), levels=c(1,0))
data$X_rain <- factor(data$X_rain,labels=c("YES", "NO"), levels=c(0,1))
data$X_thunder <- factor(data$X_thunder,labels=c("YES", "NO"), levels=c(1,0))
data$X_tornado <- factor(data$X_tornado,labels=c("YES", "NO"), levels=c(1,0))
data$X_vism = as.integer(data$X_vism)
data$X_pressurem = as.integer(data$X_pressurem)
dplyr::glimpse(data)
#count of NA Values
sapply(data, function(x) sum(is.na(x)))

summary(data)

#drop data with maximum(90%) missing values
drop = c("X_heatindexm","X_precipm","X_wgustm","X_windchillm")
data = data[,!(names(data) %in% drop)]

#Finding missing Text Values
library(mefa)
data$X_conds = fill.na(data$X_conds)
data$X_wdire = fill.na(data$X_wdire)
sapply(data, function(x) sum(is.na(x)))
#Format data time stamp and split the data
data$hour <- as.integer(format(as.POSIXct(strptime(data$datetime_utc,"%Y%m%d-%H:%M",tz="")) ,format = "%H"))
data$year <- as.integer(format(as.POSIXct(strptime(data$datetime_utc,"%Y%m%d-%H:%M",tz="")) ,format = "%Y"))
#Remove orignal datatimestamp variable
data= data[,c(-1)]

summary(data)
#manually entering value as there is some issue with date-time conversion
#conversion function by referring dataset

#Finding location of NA's
which(is.na(data$year))
data[3002,c("year","hour")] = c(1997,00)
data[12171,c("year","hour")] = c(1999,00)
data[24945,c("year","hour")] = c(2001,00)
data[33563,c("year","hour")] = c(2002,00)
data[41984,c("year","hour")] = c(2003,00)
data[56908,c("year","hour")] = c(2004,00)
data[56909,c("year","hour")] = c(2004,01)

hist(data$X_dewptm)
data$X_dewptm[is.na(data$X_dewptm)] <- median(data$X_dewptm, na.rm = TRUE)
data$X_hum[is.na(data$X_hum)] <- median(data$X_hum, na.rm = TRUE)
sapply(data, function(x) sum(is.na(x)))

#Filling all missing values with median and hence we are first removing text 
#columns so that they are replaced by factor values
exclude = c("X_wdire","X_conds")
value = data[,!(names(data) %in% exclude)]
library(dplyr)
value = value %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
#checking NA values after cleaning
sapply(value, function(x) sum(is.na(x)))
summary(value)
str(data)

data = cbind(value,data$X_wdire ,data$X_conds)
sapply(data, function(x) sum(is.na(x)))
#write.csv(data,"Final.csv")

#renaming Columns
colnames(data)
colnames(data)[1:17] =  c("Dew","Fog","Hail","Humidity","Pressure","Rain","Snow","Temprature","Thunder","Tornado","Viscocity","wind_zone","wind_speed","Hour","Year","WindDirection","Conditions")

######################## Correlation ###############################################
#Building correlation plot among variables
library(corrplot)
numeric <- names(select_if(data, is.numeric))
cor_df <- as.matrix(data[numeric])
cor_mat <- cor(as.matrix(cor_df))
cor_mat
#Visualising the correlation matrix
corrplot(cor_mat, method = "pie",type = "lower", order = "FPC", tl.col = "black",sig.level=0.05 ,tl.srt = 45)
str(data)
summary(data)
#Performing Multiple Linear Regression Model----------------------------------------------
df = data[,c(-16,-17)]
str(df)
#Transforming the dataset before Analysis
df$Fog = as.factor(df$Fog)
df$Hail= as.factor(df$Hail)
df$Rain= as.factor(df$Rain)
df$Snow= as.factor(df$Snow)
df$Thunder= as.factor(df$Thunder)
df$Tornado= as.factor(df$Tornado)
#Splitting Test and Train data
set.seed(123)
sample = sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]
#Regression Base Model----------------------------------------------
base_model = lm(Dew ~ . ,data = df )
summary(base_model)
cat("RMSE value : " ,rmse(base_model$residuals))
cat("MAE Value : " ,mae(base_model$residuals))
cat("RSS Value is :",sigma(base_model)/mean(df$Dew))
#Forward Step--------------------------------------------------
Step_fwd_model <- step(lm(Dew ~ ., data = df ),direction = "forward")
summary(Step_fwd_model)
cat("RMSE value : " ,rmse(Step_fwd_model$residuals))
cat("MAE Value : " ,mae(Step_fwd_model$residuals))
cat("RSS Value is :",sigma(Step_fwd_model)/mean(df$Dew))
#backward Step--------------------------------------------------
Step_bk_model <- step(lm(Dew ~ ., data = df) ,direction = "backward")
summary(Step_bk_model)
cat("RMSE value : " ,rmse(Step_bk_model$residuals))
cat("MAE Value : " ,mae(Step_bk_model$residuals))
cat("RSS Value is :",sigma(Step_bk_model)/mean(df$Dew))

# Other useful functions
 coefficients(Step_bk_model) # model coefficients
# confint(Step_bk_model, level=0.95) # CIs for model parameters
# fitted(Step_bk_model) # predicted values
# residuals(Step_bk_model) # residuals
#anova(Step_bk_model) # anova table
# vcov(Step_bk_model) # covariance matrix for model parameters
# influence(Step_bk_model) # regression diagnostics
vif(Step_bk_model)


# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(Step_bk_model)
rmse(Step_bk_model$residuals)
#comparing Model
anova(base_model, Step_bk_model)


#Evaluating the model based on K Fold Cross Validation----------------------------------
#K Fold Cross Validation Comparing Model efficiency
library(DAAG)
model_cv=cv.lm(data=df, Step_bk_model, m=3) # 3 fold cross-validation
summary(model_cv)
# Define training control validation------------------
set.seed(123) 
#To Resample the data upto 10 times with 3 k-fold cross validation
train.control <- trainControl(method = "cv", number = 10,repeats = 3)
# Train the model
model<- train(Dew ~., data = df, method = "lm",trControl = train.control)
# Summarize the results
summary(model)
#Implementing Decision tree classification Model----------------------------------------
library(rpart)
dt.model = rpart(Fog~.,data = df)
dt.model
#Predictions
dtree.predicted = predict(dt.model, test ,type="c")

#Confusion Matrix to Evalute Model
library(caret)
confusionMatrix(dtree.predicted,test$Fog)
#Find Area under the curve
roc(as.integer(dtree.predicted),as.integer(test$Fog))
#plot ROC Curve
dt.roc = plot(roc(test$Fog, as.integer(dtree.predicted), direction="<"),
               col="red", lwd=3, main="ROC Curve for Decison tree")
#Visualize the tree
library(rpart.plot)
library(caTools)
prp(tree)

#Linear Discriminent Analysis(LDA)-------------------------------
library(MASS)
l <- lda(formula = Fog ~ ., data = df, prior = c(1,1)/2) 
l$prior         
l$counts
l$means
l$scaling
l$svd #Singular values

library(MASS)
# Fit the model
model <- lda(Fog~., data = train)
model
# Make predictions
predictions <- model %>% predict(test)
names(predictions)
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

# Model accuracy
mean(predictions$class==test$Fog)

#Evaluation Matrix
confusionMatrix(predictions$class,test$Fog)
roc(as.integer(predictions$class),as.integer(test$Fog)) #calculate AUC
lda.data <- cbind(train, predict(model)$x)
ggplot(lda.data, aes(LD1)) +
  geom_point(aes(y = Fog))
#Plot ROC Curve
lda.roc = plot(roc(test$Fog, as.integer(predictions$class), direction="<"),
               col="red", lwd=3, main="ROC Curve for LDA")
#SVM ---------------------------------------------------------------------------
#Predicting
library(e1071)
svm_classifier <- svm(Fog ~., data=train)
summary(svm_classifier)
svm_pred = predict(svm_classifier,test)
confusionMatrix(svm_pred, test$Fog, dnn = c("Prediction", "Reference"))
roc(as.integer(svm_pred),as.integer(test$Fog))
svm.roc = plot(roc(test$Fog, as.integer(svm_pred), direction="<"),
              col="red", lwd=3, main="ROC Curve for SVM")
#Random Forest -----------------------------------------------------
library(randomForest)
rf.model = randomForest(Fog ~., data=train,mtry=6,ntree=500)
plot(rf.model)

# To check important variables
importance(rf.model)        
varImpPlot(rf.model)  #Plot random forest      
pred<-predict(rf.model,test)
confusionMatrix(pred,test$Fog)
roc(as.integer(pred),as.integer(test$Fog)) #check area under the curve
library(pROC)
rf.roc = plot(roc(test$Fog, as.integer(pred), direction="<"),
     col="yellow", lwd=3, main="ROC Curve for RF")
#Plotting the Error vs Number of Trees Graph.
plot(rf.model)

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
#--------------------------------------Plot Combined ROC ----------------------------------
plot(rf.roc,col="darkred")
plot(svm.roc,col="darkgreen",add=TRUE)
plot(lda.roc,col="blue",add=TRUE)
plot(dt.roc,col="orange",add=TRUE)
plot(knn.roc,col="black",add=TRUE)
legend(0.4,0.60, c('svm','decision tree','KNN','rf','lda'),cex=0.7,lty=c(6,1),bty = 5,
       lwd=c(4,1),col=c('darkred','darkgreen','blue','orange','black'))

str(value)
str(df)
