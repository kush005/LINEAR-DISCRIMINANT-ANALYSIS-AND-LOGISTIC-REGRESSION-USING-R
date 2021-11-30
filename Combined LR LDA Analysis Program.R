read.csv("Inclination Understanding Survey2.csv")
data<-read.csv("Inclination Understanding Survey2.csv")
View(data)
str(data)

data$v11<-as.factor(data$v11)
data$Age.<-as.factor(data$Age.)
data$Gender.<-as.factor(data$Gender.)
data$Marital.status.<-as.factor(data$Marital.status.)
data$Location.= as.factor(data$Location.)
data$Occupation<-as.factor(data$Occupation)
data$Income.<-as.factor(data$Income.)
str(data)
Data1<-data[,-c(1,19)]
str(Data1)


dim(Data1)
#split the data into training and testing
set.seed(550)
Samplesize<-floor(0.66*nrow(Data1))
set.seed(5)
traind<-sample(seq_len(nrow(Data1)),size=Samplesize)
TrainData<-Data1[traind,]
TestData<-Data1[-traind,]
library(caret)
library(MLeval)
#modeling the data to Training data
Control<-trainControl(method="cv",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
#logistic regression Model
ModelLog<-train(v11~.,method='glm',data=TrainData,trControl=Control)
TPredLog<-predict(ModelLog,TestData,type = "raw")
TPredLog
TestData$v11
#confusionMatrix
CML<-table(TPredLog,TestData$v11,dnn = c("Predicted","Actual"))
CML
confusionMatrix(CML)

#LDA
read.csv("Inclination Understanding Survey3.csv")
data3<-read.csv("Inclination Understanding Survey3.csv")
View(data3)
str(data3)
data3$v11= as.factor(data3$v11)
str(data3)
Data4<-data3[,-c(1,19)]
str(Data4)


dim(Data4)
set.seed(550)
Samplesize1<-floor(0.66*nrow(Data4))
set.seed(5)
traind1<-sample(seq_len(nrow(Data4)),size=Samplesize1)
TrainData1<-Data4[traind1,]
TestData2<-Data4[-traind1,]

Control1<-trainControl(method="cv",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
ModelLDA<-train(v11~.,method='lda',data=TrainData1,trControl=Control1)
TPredLDA<-predict(ModelLDA,TestData2,type = "raw")
CML1<-table(TPredLDA,TestData2$v11,dnn = c("Predicted","Actual"))
CML1
confusionMatrix(CML1)
ROC1 = evalm(list(ModelLog,ModelLDA),gnames = c("Log","LDA"))
ROC1
