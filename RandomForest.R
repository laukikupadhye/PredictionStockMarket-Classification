data <- read.csv(file.choose(), header = TRUE)
str(data)

set.seed(122)
ind<-sample(3,nrow(data),replace = TRUE,prob = c(0.3,0.1,0.6))
train<-data[ind==1,]
test<-data[ind==2,]
#random forest
library(randomForest)

set.seed(177)
#find best number of trees
tuneRF(train[,-8],train[,8],
       stepFactor = 0.5,
       plot=T,
       ntreeTry = 100,
       trace=T,
       improve = 0.05)

set.seed(177)
model<-randomForest(class~ +low+high+open+close+volume,
				data = train,
                ntree=13,
                mtry=3,
                importance=T,
                proximity=T)
print(model)
attributes(model)
model
p1<-predict(model, train)
library(caret)
confusionMatrix(p1,train$class)

p2<-predict(model,test)
confusionMatrix(p2,test$class)

