library("mlbench")
library("caret")
rm(list=ls(all=TRUE))
data("HouseVotes84")
1:ncol(HouseVotes84)
colnames(HouseVotes84) <- sapply(1:ncol(HouseVotes84),function(x) paste0("col",x))
colnames(HouseVotes84)[1]<-"Class"
colnames(HouseVotes84)[2]<- "handicapped-infants"
colnames(HouseVotes84)[3]<-"water-project-cost-sharing"
colnames(HouseVotes84)[4]<-"adoption-of-the-budget-resolution"
colnames(HouseVotes84)[5]<-"physician-fee-freeze"
colnames(HouseVotes84)[6]<-"el-salvador-aid"
colnames(HouseVotes84)[7]<-"religious-groups-in-schools"
colnames(HouseVotes84)[8]<-"anti-satellite-test-ban"
colnames(HouseVotes84)[9]<-"aid-to-nicaraguan-contras"
colnames(HouseVotes84)[10]<-"mx-missile"
colnames(HouseVotes84)[11]<-"immigration"
colnames(HouseVotes84)[12]<-"synfuels-corporation-cutback"
colnames(HouseVotes84)[13]<-"education-spending"
colnames(HouseVotes84)[14]<-"superfund-right-to-sue"
colnames(HouseVotes84)[15]<-"duty-free-exports"
colnames(HouseVotes84)[16]<-"export-administration-act-south-africa"
sub<-sample(nrow(HouseVotes84),floor(nrow(HouseVotes84) * .7))
train <- HouseVotes84[sub,]
test <- HouseVotes84[-sub,]
xTrain <- train[,-1]
yTrain <- train$Class
xTest <- test[,-1]
yTest <- test$Class
model <- train(xTrain,yTrain,'nb',trcontrol=trainControl(method='cv',number=10))
confusion_matrix_train <- table(predict(model$finalModel,xTrain)$class,yTrain)

print("TRAIN-CONFUSION-MATRIX")

print(confusion_matrix_train)
confusion_matrix_test  <- table(predict(model$finalModel,xTest)$class,yTest)

print("TEST-CONFUSION-MATRIX")

print(confusion_matrix_test)


