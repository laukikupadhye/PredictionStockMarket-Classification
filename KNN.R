#required libs
library(caret)
library(pROC)
library(mlbench)

data <- read.csv(file.choose(), header = TRUE)
str(data)

set.seed(122)
ind<-sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train<-data[ind==1,]
test<-data[ind==2,]

#kNN

trControl<-trainControl(method="repeatedcv",
                        number=5,
                        repeats=2)
set.seed(222)
tic<-system.time()
fit<-train(class~ +low+high+open+close+volume, data=train,method='knn',
           tuneLength = 5,
           trControl=trControl,
           preProc=c("center","scale"))
toc<-system.time()-tic

fit
plot(fit)
varImp(fit)
p1<-predict(fit,newdata = test)
confusionMatrix(p1,test$class)
