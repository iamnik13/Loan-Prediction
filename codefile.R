train<-read.csv("train.csv")
test<-read.csv("test.csv")
str(train)
str(test)

                                  #Data Cleaning

#turn binary variable Credit_History into factor
train$Credit_History <- factor(train$Credit_History, labels = c("N","Y"))
test$Credit_History  <- factor(test$Credit_History,  labels = c("N","Y"))

#convert factor variable Dependents into numeric
levels(train$Dependents)[levels(train$Dependents)=="3+"] <- "3"
train$Dependents <- as.integer(as.character(train$Dependents))
levels(test$Dependents)[levels(test$Dependents)=="3+"] <- "3"
test$Dependents  <- as.integer(as.character(test$Dependents))

#remove the id column 
train<-train[-1]

#some NA's are coded as empty strings
train$Gender[train$Gender==""] <- NA
train$Married[train$Married==""] <- NA
train$Self_Employed[train$Self_Employed==""] <- NA
test$Gender[test$Gender==""] <- NA
test$Married[test$Married==""] <- NA
test$Self_Employed[test$Self_Employed==""] <- NA

#test data does not have the empty level as train data does
train$Married <- droplevels(train$Married) 

library(ggplot2)
qplot(train$Loan_Status,train$ApplicantIncome)
qplot(train$Loan_Status, train$CoapplicantIncome)
qplot(train$Loan_Status, train$LoanAmount)


   #create a new feature of the number of Na in an observation
train_NAs <- NULL
test_NAs  <- NULL
for(i in 1:nrow(train)){ 
  train_NAs[i]<-sum(is.na(train[i, ]))
  train$NA_number <- train_NAs
}
for(i in 1:nrow(test)) {
  test_NAs[i] <- sum(is.na(test[i, ]))
  
  }

#the ratio of missing for each variable
names <- names(train)
missing<-data.frame(variable=names,missing_proportion=sapply(names,function(x) sum(is.na(train[x]))/nrow(train)))
missing #The missing rate does not exceed 10%

library(mice)
#input missing values by package mice, considering that the missing values are not MNAR(missing not at random)
trainimp <- mice(data=train,m=5,maxit = 10,method="pmm",printFlag=FALSE,seed=0817) #estimate fitting values for continuous variables using predictive mean matching of mice
newtrain <- complete(trainimp) #imput the estimated missing values
sum(is.na(newtrain)) #all missing value are imputed

testimp <- mice(data=test[-1],m=5,maxit = 10,method="pmm",printFlag=FALSE,seed=0817) 
newtest <- complete(testimp) #imput the estimated missing values for test dataset as well
sum(is.na(newtest))

newtrain$term360<-NULL
newtrain$status<-NULL
newtrain$Total_Income<-NULL

                                  #Model building and tuning

#For this problem, we will use Random Forest as our prediction model.
library(randomForest)
set.seed(817)
original_rf <- randomForest(Loan_Status~ ., newtrain[-c(13:17)])
original_rf

# base model without feature engineering has an OOB error rate of 18.89%
feature_engineered_rf <- randomForest(Loan_Status~., newtrain, mtry=3)
feature_engineered_rf
# base model with additional variables has a better OOB error rate 18.08% 
# use the same mtry as original_rf

                                 #Tuning Model

set.seed(817)
tune_grid <- expand.grid(mtry=c(1:10), ntree=c(1000,1500,2000,2500)) #expand a grid of parameters
      mtry<- tune_grid[[1]]
     ntree<- tune_grid[[2]] #using vectors instead of dataframe to subset is faster in for loop
       OOB<- NULL #use to store calculated OOB error estimate
for(i in 1:nrow(tune_grid)){
  rf <- randomForest(Loan_Status~. ,newtrain, mtry=mtry[i], ntree=ntree[i])
  confusion<-rf$confusion
  temp<-(confusion[2]+confusion[3])/614 #calculate the OOB error estimate
  OOB<-append(OOB,temp)
}
tune_grid$OOB <- OOB
head(tune_grid[order(tune_grid["OOB"]), ], 4) #order the results 

#Tuning Model

set.seed(817)
tune_grid<-expand.grid(mtry=c(1:10), ntree=c(1000,1500,200,2500)) #expand a grid of parameters
mtry<-tune_grid[[1]]
ntree<-tune_grid[[2]] #using vectors instead of dataframe to subset is faster in for loop
OOB<-NULL #use to store calculated OOB error estimate
for(i in 1:nrow(tune_grid)){
  rf<-randomForest(Loan_Status~. ,newtrain, mtry=mtry[i], ntree=ntree[i])
  confusion<-rf$confusion
  temp<-(confusion[2]+confusion[3])/614 #calculate the OOB error estimate
  OOB<-append(OOB,temp)
}
tune_grid$OOB<-OOB
head(tune_grid[order(tune_grid["OOB"]), ], 4) #order the results 

#Tuning Model

set.seed(817)
tune_grid<-expand.grid(mtry=c(1:10), ntree=c(1000,1500,2000,2500)) #expand a grid of parameters
mtry<-tune_grid[[1]]
ntree<-tune_grid[[2]] #using vectors instead of dataframe to subset is faster in for loop
OOB<-NULL #use to store calculated OOB error estimate
for(i in 1:nrow(tune_grid)){
  rf<-randomForest(Loan_Status~. ,newtrain, mtry=mtry[i], ntree=ntree[i])
  confusion<-rf$confusion
  temp<-(confusion[2]+confusion[3])/614 #calculate the OOB error estimate
  OOB<-append(OOB,temp)
}
tune_grid$OOB<-OOB
head(tune_grid[order(tune_grid["OOB"]), ], 4) #order the results 

final_rf <- randomForest(Loan_Status~. ,newtrain, mtry=3, ntree=1000)

final_rf
#             It seems that the model has the best accuracy for mtry=3 and ntree=1000, 
#             yielding 18.8% OOB error rate which is close to the default parameters. 
#             This is exceptable since random forest is known for its 
#             high performance even using default parameters.


                               #Predicting with the model

#rf
predictions <- unname(predict(final_rf,newtest[]))
solution <- data.frame(Loan_ID=test[1], Loan_Status=predictions) #predict the test set
write.csv(solution,"Loan Status Solution.csv") #write the predicted result into solution checker file

varImpPlot(final_rf)

