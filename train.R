library(mlr)
train <- read.csv('D:/R Projects/Loan Prediction/train.csv', na.strings = c(""," ", NA ))

test <- read.csv('D:/R Projects/Loan Prediction/test.csv', na.strings = c("", " ", NA))

summarizeColumns(train)
summarizeColumns(test)

barplot(table(train$Loan_Status))

prop.table(table(train$Loan_Status))

par(mfrow=c(1,2))

barplot(table(train$Gender), main = "train")
barplot(table(test$Gender), main = "test")

prop.table(table(train$Gender))
prop.table(table(test$Gender))


par(mfrow=c(1,2))
barplot(table(train$Married), main='train set')
barplot(table(test$Married), main='test set')

prop.table(table(train$Married))
prop.table(table(test$Married))

levels(train$Dependents)

par(mfow=c(1,2))
barplot(table(train$Dependents), main='train set')
barplot(table(test$Dependents), main='test set')

prop.table(table(train$Dependents))
prop.table(table(test$Dependents))

par(mfrow=c(1,2))
barplot(tabl(train$Education), main='train set')
barplot(table(test$Education), main='test set')

prop.table(table(train$Education))
prop.table(table(test$Education))

par(cmfrow=c(1,2))
barplot(table(train$Self_Employed), main='train set')
barplto(table(test$Self_Employed),main='test set')

prop.table(table(train$Self_Employed))
prop.table(table(test$Self_Employed))

par(mfrow=c(1,2))
boxplot(train$ApplicantIncome, train$CoapplicantIncome, names=c("App Income","Coapp Income"), main='train set')
boxplot(test$ApplicantIncome, test$CoapplicantIncome, names=c("App Income","Coapp Income"), main='test set')

par(mfrow=c(1,2))
boxplot(table(train$LoanAmount), main='train set')
boxplot(table(test$LoanAmount), main='test set')

par(mfrow=c(1,2))
hist(train$Loan_Amount_Term, breaks=500, main='train set')
hist(test$Loan_Amount_Term, breaks=500, main='test set')

summary(train$Loan_Amount_Term)
summary(test$Loan_Amount_Term)

par(mfrow=c(1,2))
train$Credit_History <- as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)
barplot(table(train$Credit_History), main='train set')
barplot(table(test$Credit_History), main='test set')

prop.table(table(train$Credit_History))
prop.table(table(test$Credit_History))

par(mfrow=c(1,2))
barplot(table(train$Property_Area),main='train set')
barplot(table(test$Property_Area),main='test set')

prop.table(table(train$Property_Area))
prop.table(table(test$Property_Area))

library(ggplot2)
ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#FF6666") + 
              facet_grid(.~Gender) + ggtitle("Loan Status by Gender of Applicant")

ggplot(train, aes(x=Loan_Status))+ geom_bar(fill="#FF2222") + 
  facet_grid(.~Married) + ggtitle("Loan Status by Marital Status of Applicant")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#86DBC4") +
  facet_grid(.~Dependents) + ggtitle("Loan Status by number of Dependents of Applicant")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#E3E323") +
  facet_grid(.~Education) + ggtitle("Loan Status by Education of Applicant")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#717d7e") +
  facet_grid(.~Self_Employed) + ggtitle("Loan Status by Employment status of Applicant")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#138d75") +
  facet_grid(.~Loan_Amount_Term) + ggtitle("Loan Status by terms of Loan")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#784212") +
  facet_grid(.~Credit_History) + ggtitle("Loan Status by Credit History of Applicant")

ggplot(train, aes(x=Loan_Status)) + geom_bar(fill="#273746") +
  facet_grid(.~Property_Area) + ggtitle("Loan Status by Property area")

ggplot(train, aes(x=Loan_Status, y=ApplicantIncome)) + geom_boxplot() + ggtitle("Loan Status by Applicant")

ggplot(train, aes(x=Loan_Status, y=CoapplicantIncome)) + geom_boxplot() + ggtitle("LOan Status by Coapplicant")

ggplot(train, aes(x=Loan_Status, y=LoanAmount)) + geom_boxplot() + ggtitle("Loan Status by Loan Amount")

#Tidying the data - filling in missing values
alldata <- rbind(train[,2:12], test[,2:12])

library(ggplot2)
ggplot(data = alldata[alldata$ApplicantIncome<20000, ], aes(ApplicantIncome, fill=Married)) + geom_histogram() + 
  facet_grid(Gender~.)

ggplot(data = alldata[alldata$ApplicantIncome < 20000, ], aes(CoapplicantIncome, fill=Married)) + 
  facet_grid(Gender~.) + geom_histogram()


library(plyr)
alldata2 <- mutate(alldata, TotalIncome= ApplicantIncome + CoapplicantIncome)

ggplot(alldata2, aes(TotalIncome, fill=Married)) + geom_histogram(position = "dodge",bins=20) + facet_grid(Gender~., margins= FALSE,as.table = TRUE)

#Furthermore, it seems reasonable to 
#impute marital status as “No” when the coapplicant income is zero, 
#and “Yes”, otherwise.

alldata2$Married[is.na(alldata2$Married) & alldata2$CoapplicantIncome==0]<-"No"
alldata2$Married[is.na(alldata2$Married)]<- "Yes"

#2.Gender and dependents
alldata2[is.na(alldata2$Gender) & is.na(alldata2$Dependents),]
      
            #This applicant is not married but has higher income than the coapplicant.
            #I’ll impute this one as “Male”. So all the other 
            #missing observations have only one of these variables missing. 
            #Let’s take a look at the rows with missing number of dependents:

alldata2$Gender[is.na(alldata2$Gender) & is.na(alldata2$Dependents)] <- "Male"
ggplot(alldata2, aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married)


library(rpart)
library(rattle)

alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Married=="No"] <- "0"

mm <- alldata2[(alldata2$Gender=="Male" & alldata2$Married=="Yes"), c(3,6:9,11)]
mmtrain <- mm[!is.na(mm$Dependents), ]
mmtest <- mm[is.na(mm$Dependents), ]
depfit <- rpart(data=mmtrain, Dependents~., xval=3)
prp(depfit)
fancyRpartPlot(depfit)


p<-predict(depfit,mmtrain,type="class")
acc=sum(p==mmtrain[,1])/length(p)
acc


alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Gender=="Male" & alldata2$Married == "Yes"]<- predict(depfit,newdata=mmtest,type="class")

gtrain<-alldata2[!is.na(alldata2$Gender),1:7]
gtest<-alldata2[is.na(alldata2$Gender),1:7]
genFit<-rpart(data=gtrain,Gender~.,xval=3)
fancyRpartPlot(genFit)

p<-predict(genFit,gtrain,type="class")
acc<-sum(p==gtrain[,1])/length(p)
acc

alldata2$Gender[is.na(alldata2$Gender)]<-predict(genFit,gtest,type="class")

alldata2$Self_Employed[is.na(alldata$Self_Employed)] <- "No"

library(car)
alldata2$Credit_History<-recode(alldata2$Credit_History,"NA=2")

ltrain<-alldata2[!is.na(alldata2$LoanAmount) & alldata2$LoanAmount<500,c(1:8,10)]
ltest <- alldata2[is.na(alldata2$LoanAmount),c(1:8,10)]
loanFit <- glm(data=ltrain,LoanAmount~.,na.action=na.exclude)
#impute
alldata2$LoanAmount[is.na(alldata2$LoanAmount)] <- predict(loanFit,newdata=ltest)

alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)
print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())

alldata2$Loan_Amount_Term[is.na(alldata2$Loan_Amount_Term)]<-"360"
alldata2$Loan_Amount_Term <- recode(alldata2$Loan_Amount_Term,"'350'='360';'6'='60'")


#Training and Predicting Loan status

#Now that there are no missing values it will be possible to build a model to predict the target variable. The first job is to reorganize the data as training and test sets.

newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])

#bogus Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(Loan_Status,alldata2[615:981,])

#create task
trainTask <- makeClassifTask(data = newtrain,target = "Loan_Status")
testTask <- makeClassifTask(data = newtest, target = "Loan_Status")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")



#Now we are ready for our first attempt. Let’s start with a decision tree model.

tree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
treepars <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#try 100 different combinations of values
tpcontrol <- makeTuneControlRandom(maxit = 100L)

#hypertune the parameters
rm(acc)
set.seed(11)
treetune <- tuneParams(learner = tree, resampling = set_cv, task = trainTask, par.set = treepars, control = tpcontrol, measures = acc)
treetune

#using hyperparameters for modeling
tunedtree <- setHyperPars(tree, par.vals=treetune$x)

#train the model
treefit <- train(tunedtree, trainTask)
par(mfrow=c(1,1))
fancyRpartPlot(getLearnerModel(treefit))


#make predictions
treepred <- predict(treefit, testTask)

#create a submission file
submit1 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = treepred$data$response)
# write.csv(submit1, "sol1.csv",row.names = F)

#That gave 0.791667 accuracy on the public leaderboard. The model is actually very simple and bases it’s predictions only on three of the variables.

#Let’s see if a random forest can improve:
  
  #create a learner
  rf <- makeLearner("classif.randomForest", predict.type = "response"
                    , par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

#set tunable parameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 2, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#let's do random search for 100 iterations
rancontrol <- makeTuneControlRandom(maxit = 100L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
set.seed(11)
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)
#cv accuracy
rf_tune$y

#best parameters
rf_tune$x

#using hyperparameters for modeling
tunedrf <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(tunedrf, trainTask)
getLearnerModel(rforest)

#make predictions
rfmodel <- predict(rforest, testTask)

#submission file
submit2 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = rfmodel$data$response)
# write.csv(submit2, "sol2.csv",row.names = F)

submit<-cbind(submit1$Loan_Status,submit2$Loan_Status)
sum(submit[,1]==submit[,2])

library(randomForest)
varImpPlot(rforest$learner.model)
