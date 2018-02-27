train <- read.csv("content_train.tsv",sep = '\t')
test <- read.csv("content_test.tsv",sep='\t')


x<-na.omit(train$content_1)
summary(train)
train_content1<-train[,c(1,2,11:27)]
train_content1<-na.omit(train_content1)
summary(train_content1)
library(caTools)
set.seed(88)
split <- sample.split(train_content1$content_1,SplitRatio = 0.7)
Train <-subset(train_content1,split==TRUE)
Test <-subset(train_content1,split==FALSE)
table(Test$content_1)
hist(train_content1$express.no.transactions)
hist(log(train_content1$express.total.spend))
hist(train_content1$metro.no.transactions)
hist(log(train_content1$metro.total.spend))
hist(train_content1$metro.no.transactions)
hist(train_content1$superstore.no.transactions)
hist(log(train_content1$superstore.total.spend))
plot(train_content1$extra.total.spend,cex=0.4,col=as.factor(train_content1$content_1))
boxplot((train_content1[,3:16]))
t.test(express.total.spend~content_1,data=train_content1)
chisq.test(train_content1$content_1,train_content1$gender)
train_content1$express.total.spend<-log(train_content1$express.total.spend)
train_content1$metro.total.spend<-log(train_content1$metro.total.spend)
train_content1$superstore.total.spend<-log(train_content1$superstore.total.spend)


prop.table(table(train_content1$content_1,train_content1$gender),margin = 2)

PenaltyMatrix = matrix(data = c(0,1,50,0),nrow = 2,ncol = 2,byrow = T)
PenaltyMatrix
library(rpart)
library(rpart.plot)
train_content1$county<- as.character(train_content1$county)
model<-rpart(content_1~express.no.transactions+express.total.spend+metro.no.transactions+metro.total.spend+superstore.no.transactions+superstore.total.spend+extra.no.transactions+extra.total.spend+fandf.no.transactions+fandf.total.spend+petrol.no.transactions+petrol.total.spend+direct.no.transactions+direct.total.spend+gender+affluency,data=Train,cp=0.004,method='class',parms=list(loss=PenaltyMatrix))
pred_train <- predict(model,type='class')
table(Train$content_1,pred_train)
prp(model)
table(train_content1$county)
pred_test <- predict(model,newdata = Test,type='class')
table(Test$content_1,pred_test)
library(caret)

numFolds = trainControl(method = "cv",number = 10)
cpGrid <- expand.grid(cp = seq(0.00001,0.001,0.00001))

model_cv<-train(as.factor(content_1)~express.no.transactions+express.total.spend+metro.no.transactions+metro.total.spend+superstore.no.transactions+superstore.total.spend+extra.no.transactions+extra.total.spend+fandf.no.transactions+fandf.total.spend+petrol.no.transactions+petrol.total.spend+direct.no.transactions+direct.total.spend+gender+affluency,data = Train,trControl=numFolds,tuneGrid=cpGrid,method="rpart",maximize=F,metric="Kappa")

penaltyerror<-function(data,lev=NULL,model=NULL)
{
  error_mat<-as.matrix(table(data$obs,data$pred))*PenaltyMatrix
  error<-sum(error_mat)/nrow(data)
  names(error)<-c("Penalty Error")
  error
}
plot(model_cv)
prp(model_cv$finalModel)

pred_train <-predict(model_cv)
table(Train$content_1,pred_train)
pred_test <-predict(model_cv,newdata = Test)
table(Test$content_1,pred_test)

weight = getinfo(dtrain,'label')*10
table(Train$content_1)

model_rf<-randomForest(as.factor(content_1)~express.no.transactions+express.total.spend+metro.no.transactions+metro.total.spend+superstore.no.transactions+superstore.total.spend+extra.no.transactions+extra.total.spend+fandf.no.transactions+fandf.total.spend+petrol.no.transactions+petrol.total.spend+direct.no.transactions+direct.total.spend+gender+affluency,data=Train,sampsize = c(900,351),strata=Train$content_1,cutoff=c(0.7,0.3))

pred_rf <-predict(model_rf,newdata = Test)
table(Test$content_1,pred_rf)

library(xgboost)
library(Matrix)

sparse_matrix<-sparse.model.matrix(content_1~.-1-customer.id-county,data = Train)
sparse_matrix@Dim

dtrain<-xgb.DMatrix(data=sparse_matrix,label=Train$content_1)

sparse_matrix_test<-sparse.model.matrix(content_1~.-1-customer.id-county,data = Test)

dtest<-xgb.DMatrix(data=sparse_matrix_test,label=Test$content_1)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "penalty_error", value = err))
}
watchlist=list(train = dtrain, test=dtest)

params=list(eta=0.01,max_depth=8,objective="binary:logistic")
model_xgb<-xgb.train(params = params,data = dtrain,verbose = 1,watchlist = watchlist,nrounds=1000,nthread=8,maximize=F,early_stopping_rounds = 100,scale_pos_weight=91,feval=evalerror)

pred_xgb<-predict(model_xgb,newdata = dtest)
table(Test$content_1,pred_xgb>=0.5)

pred_xgb_train <- predict(model_xgb,newdata = dtrain)
table(Train$content_1,pred_xgb_train>=0.5)

