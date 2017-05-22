
install.packages('e1071', dependencies = TRUE)
library(e1071)
x_train=read.table("http://www.amlbook.com/data/zip/features.train")
colnames(x_train)=c("digit", "sysmetry","intensity") 

x_test=read.table("http://www.amlbook.com/data/zip/features.test")
colnames(x_test)=c("digit", "sysmetry","intensity") 




#one verses all 
trainError=function(x){
  
  Etrain=vector("numeric")
  Etest=vector("numeric")
  
  for(e in x){
    y_train=NA
    y_train[x_train$digit==e]=1
    y_train[x_train$digit!=e]=-1
    y_train <- as.factor(y_train)
    length(y_train[is.na(y_train)])
    
    y_test=NA
    y_test[x_test$digit==e]=1
    y_test[x_test$digit!=e]=-1
    y_test <- as.factor(y_test)
    length(y_test[is.na(y_test)])
    
    
    model_0  <- svm(y_train~., data = x_train[,-1], type="C-classification", 
                    kernel = "polynomial",gamma=1, coef0=1,degree=2, cost = 0.01)
    print(summary(model_0))
    
    prediction.train <- predict(model_0, x_train[,-1])
    tab_train=table(pred = prediction.train, true = y_train)
    errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)
    Etrain=c(Etrain,errorRate)
    
    prediction.test <- predict(model_0, x_test[,-1])
    tab_test <- table(pred = prediction.test, true = y_test)
    errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)
    Etest=c(Etest,errorRate)

   plot(model_0,x_train)    
  }
 list(Etrain,Etest) 
  
}

choice1=c(0,2,4,6,8)
trainError(choice1)

choice2=c(1,3,5,7,9)
trainError(choice2)



########## 1 Vs 5 , Q = 2
x_train_1 <- subset(x_train , x_train$digit ==1 | x_train$digit ==5)
y_train=NA
y_train[x_train_1$digit==1]=1
y_train[x_train_1$digit==5]=-1
y_train <- as.factor(y_train)
length(y_train[is.na(y_train)])

x_test_1 <- subset(x_test , x_test$digit ==1 | x_test$digit ==5)
y_test=NA
y_test[x_test_1$digit==1]=1
y_test[x_test_1$digit==5]=-1
y_test <- as.factor(y_test)
length(y_test[is.na(y_test)])


Error=function(x){
    
    Etrain=vector("numeric")
    Etest=vector("numeric")
    
    for(e in x){
    model_1  <- svm(y_train~., data = x_train_1[,-1], type="C-classification", 
                    kernel = "polynomial",gamma=1, coef0=1,degree=2, cost = e)
    print(summary(model_1))
    
    prediction.train <- predict(model_1, x_train_1[,-1])
    tab_train=table(pred = prediction.train, true = y_train)
    errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)
    Etrain=c(Etrain,errorRate)
    
    prediction.test <- predict(model_1, x_test_1[,-1])
    tab_test <- table(pred = prediction.test, true = y_test)
    errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)
    Etest=c(Etest,errorRate)
    
    plot(model_1,x_train_1)    
    }
    list(Etrain,Etest) 
  
}

choice3=c(0.001, 0.01, 0.1, 1)
Error(choice3)

###  1 Vs 5 , Q = 2 second list of c
Error=function(x){
  
  Etrain=vector("numeric")
  Etest=vector("numeric")
  
  for(e in x){
    model_1  <- svm(y_train~., data = x_train_1[,-1], type="C-classification", 
                    kernel = "polynomial",gamma=1, coef0=1,degree=2, cost = e)
    print(summary(model_1))
    
    prediction.train <- predict(model_1, x_train_1[,-1])
    tab_train=table(pred = prediction.train, true = y_train)
    errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)
    Etrain=c(Etrain,errorRate)
    
    prediction.test <- predict(model_1, x_test_1[,-1])
    tab_test <- table(pred = prediction.test, true = y_test)
    errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)
    Etest=c(Etest,errorRate)
    
    plot(model_1,x_train_1)    
  }
  list(Etrain,Etest) 
  
}

choice3_1=c(0.0001, 0.001, 0.01, 1)
Error(choice3_1)


########## 1 Vs 5 , Q = 5

Error=function(x){
  
  x_train_1 <- subset(x_train , x_train$digit ==1 | x_train$digit ==5)
  y_train=NA
  y_train[x_train_1$digit==1]=1
  y_train[x_train_1$digit==5]=-1
  y_train <- as.factor(y_train)
  length(y_train[is.na(y_train)])
  
  x_test_1 <- subset(x_test , x_test$digit ==1 | x_test$digit ==5)
  y_test=NA
  y_test[x_test_1$digit==1]=1
  y_test[x_test_1$digit==5]=-1
  y_test <- as.factor(y_test)
  length(y_test[is.na(y_test)])
  
  
  Etrain=vector("numeric")
  Etest=vector("numeric")
  
  for(e in x){
    model_1  <- svm(y_train~., data = x_train_1[,-1], type="C-classification", 
                    kernel = "polynomial",gamma=1, coef0=1,degree=5, cost = e)
    print(summary(model_1))
    
    prediction.train <- predict(model_1, x_train_1[,-1])
    tab_train=table(pred = prediction.train, true = y_train)
    errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)
    Etrain=c(Etrain,errorRate)
    
    prediction.test <- predict(model_1, x_test_1[,-1])
    tab_test <- table(pred = prediction.test, true = y_test)
    errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)
    Etest=c(Etest,errorRate)
    
    plot(model_1,x_train_1)    
  }
  list(Etrain,Etest) 
  
}

choice4=c(0.0001, 0.001, 0.01, 1)
Error(choice4)



##########10-fold cross validation for the polynomial kernel

tune.out <- tune(svm, train.x=x_train_1[,-1], train.y=y_train, type="C-classification", 
             kernel = "polynomial",gamma=1, coef0=1,degree=2,
                 ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1)), 
             tune.control(random = FALSE, 
        nrepeat = 100, repeat.aggregate = mean,
                  sampling = "cross", sampling.aggregate = mean,
                   sampling.dispersion = sd,
                      cross = 10, best.model = TRUE,
                  performances = TRUE, error.fun = NULL))
summary(tune.out)



##### kernal=radian

GError=function(x){
  Etrain=vector("numeric")
  Etest=vector("numeric")
  for(e in x){
    model_2  <- svm(y_train~., data = x_train_1[,-1], type="C-classification", 
                    kernel = "radial",gamma=1,  cost = e)
    print(summary(model_2))
    
    prediction.train <- predict(model_2, x_train_1[,-1])
    tab_train=table(pred = prediction.train, true = y_train)
    errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)
    Etrain=c(Etrain,errorRate)
    
    prediction.test <- predict(model_2, x_test_1[,-1])
    tab_test = table(pred = prediction.test, true = y_test)
    errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)
    Etest=c(Etest,errorRate)
    
    plot(model_2,x_train_1)    
   
  }
  list(Etrain,Etest)
}

choice5=c(0.01,1,100,10000,1000000)
GError(choice5)

model_3  <- svm(y_train~., data = x_train_1[,-1], type="C-classification", 
                kernel = "radial",gamma=1,  cost = 1000000)
print(summary(model_3))

prediction.train <- predict(model_3, x_train_1[,-1])
tab_train=table(pred = prediction.train, true = y_train)
errorRate=(sum(tab_train)-sum(diag(tab_train)))/sum(tab_train)

prediction.test <- predict(model_3, x_test_1[,-1])
tab_test = table(pred = prediction.test, true = y_test)
errorRate=(sum(tab_test)-sum(diag(tab_test)))/sum(tab_test)

