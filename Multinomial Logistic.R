data <- read.csv(file.choose(), header = TRUE)
str(data)

set.seed(122)
ind<-sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train<-data[ind==1,]
test<-data[ind==2,]
#logistic regression
require(nnet)
model<-multinom(class~ +low+high+open+close+volume, data=train)
predict_score<-predict(model,test,"probs")
predict_score
predict_class<-predict(model,test)
table(predict_class,test$class)
#misclassification error 
mean(as.character(predict_class)!=as.character(test$class))

