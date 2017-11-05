#import economic indicators data set
df <- read.csv("e:/CSC529/case2/Case2Bond1.csv")
head(df)

nrow(df)
df$GDP_M = 0
head(df)
for (i in 3:(nrow(df)-2)){
  if (i %% 3 ==0){
    df$GDP_M[i] <- df$GDP[i-1] 
  }
  if (i %% 3 ==1){
    df$GDP_M[i] <- 2/3 * df$GDP[i-2] + 1/3 * df$GDP[i+1]
  }
  if (i %% 3 ==2){
    df$GDP_M[i] <- 2/3 * df$GDP[i] + 1/3 * df$GDP[i-3]
  }
}
df$GDP_M[1] <- df$GDP[2]
df$GDP_M[2] <- df$GDP[2]
df$GDP_M[nrow(df)] <- df$GDP[nrow(df)-2]
df$GDP_M[nrow(df)-1] <- df$GDP[nrow(df)-2]

head(df)
tail(df)


#load Shibors (interest rate data set) and TF00C1 (5 year government bond) dataset
Shibors <- read.csv("e:/CSC529/case2/ShiborsVSbond.csv")
head(Shibors)

library(corrplot)
M <- cor(Shibors)
M
corrplot(M, method="pie", type = "lower")

Shibors$i_next_M = NA
Shibors
for (i in 2:nrow(Shibors)){
  if (Shibors$Shibor1Y[i] < Shibors$Shibor1Y[i-1]){
    Shibors$i_next_M[i] = "up"
  }
  else if (Shibors$Shibor1Y[i] > Shibors$Shibor1Y[i-1]){
    Shibors$i_next_M[i] = "down"
  }
  else{
    if (Shibors$Shibor3M[i] < Shibors$Shibor3M[i-1]){
      Shibors$i_next_M[i] = "up"
    }
    else if (Shibors$Shibor3M[i] > Shibors$Shibor3M[i-1]){
      Shibors$i_next_M[i] = "down"
    }
    else{
      if (Shibors$Shibor6M[i] < Shibors$Shibor6M[i-1]){
        Shibors$i_next_M[i] = "up"
      }
      else if (Shibors$Shibor6M[i] > Shibors$Shibor6M[i-1]){
        Shibors$i_next_M[i] = "down"
      }
      else{
        if (Shibors$Shibor1W[i] < Shibors$Shibor1W[i-1]){
          Shibors$i_next_M[i] = "up"
        }
        else if (Shibors$Shibor1W[i] > Shibors$Shibor1W[i-1]){
          Shibors$i_next_M[i] = "down"
        }
        else{
          if (Shibors$Shibor1M[i] < Shibors$Shibor1M[i-1]){
            Shibors$i_next_M[i] = "up"
          }
          else if (Shibors$Shibor1M[i] > Shibors$Shibor1M[i-1]){
            Shibors$i_next_M[i] = "down"
          }
          else{
            if (Shibors$ShiborO.N[i] < Shibors$ShiborO.N[i-1]){
              Shibors$i_next_M[i] = "up"
            }
            else {
              Shibors$i_next_M[i] = "down"
            }
          }
        }
      }
    }
  }
}

Shibors
nrow(Shibors)
nrow(df)

Sh <- cbind (df[2:6],df[9],Shibors[8])
sapply(sh,class)


boxplot(CPI_core. ~ i_next_M, data = sh, main= "Core CPI% VS interest rate next month",
        xlab = "interest rate next month", ylab = "Core CPI%")
boxplot(CPI_core. ~ i_next_M, data = sh, main= "Core CPI% VS interest rate next month",
        xlab = "interest rate next month", ylab = "Core CPI%")
boxplot(M2. ~ i_next_M, data = sh, main= "M2% VS interest rate next month",
        xlab = "interest rate next month", ylab = "M2%")
boxplot(M2 ~ i_next_M, data = sh, main= "M2 VS interest rate next month",
        xlab = "interest rate next month", ylab = "M2")
boxplot(PMI ~ i_next_M, data = sh, main= "PMI VS interest rate next month",
        xlab = "interest rate next month", ylab = "PMI")
boxplot(GDP_M ~ i_next_M, data = sh, main= "GDP% monthly VS interest rate next month",
        xlab = "interest rate next month", ylab = "GDP% monthly")

summary(sh)
M.sh <- cor(sh[1:6])
M.sh
corrplot(M.sh,method="pie", type = "lower")
corrplot(M.sh,method="number", type = "lower")


#20% test, 80% training
#library(rpart)
#library(caret)
set.seed(123)
ind <- sample(2, nrow(sh), replace=TRUE, prob=c(0.8, 0.2))
train <- sh[ind==1,]
test <- sh[ind==2,]

head(train)
#help(train)
library(caret)
names(getModelInfo())

#Remove M2 and CPI%
train_r <- cbind(train[2:3],train[5:7])
test_r <- cbind(test[2:3],test[5:7])
library(e1071)


#linear kernel SVM with 10 fold cross validation tuning
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
help("trainControl")
set.seed(123)
svm_l <- train(i_next_M ~ ., data = train_r, method = "svmLinear", trControl = fitControl,verbose = FALSE)
svm_l$finalModel
plot(svm_l)

pred <- predict(svm_l,test_r)
t <- table(pred,test_r$i_next_M)
#library(caret)
confusionMatrix(t)



#polynomial kernel SVM with 10 fold cross validation tuning (without M2 and CPI%)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
set.seed(123)
svm_p <- train(i_next_M ~ ., data = train_r, method = "svmPoly", trControl = fitControl,verbose = FALSE)
svm_p$finalModel
plot(svm_p)

pred <- predict(svm_p,test_r)
t <- table(pred,test_r$i_next_M)
confusionMatrix(t)


#polynomial kernel SVM with 10 fold cross validation tuning (with M2 and CPI%)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
set.seed(123)
svm_p <- train(i_next_M ~ ., data = train, method = "svmPoly", trControl = fitControl,verbose = FALSE)
svm_p$finalModel
plot(svm_p)

pred <- predict(svm_p,test)
t <- table(pred,test$i_next_M)
confusionMatrix(t)


#radial basis kernel SVM with 10 fold cross validation tuning (without M2 and CPI%)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
set.seed(123)
svm_p <- train(i_next_M ~ ., data = train_r, method = "svmRadial", trControl = fitControl,verbose = FALSE)
svm_p$finalModel
plot(svm_p)

pred <- predict(svm_p,test_r)
t <- table(pred,test_r$i_next_M)
confusionMatrix(t)


#radial basis kernel SVM with 10 fold cross validation tuning (with M2 and CPI%)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
set.seed(123)
svm_p <- train(i_next_M ~ ., data = train, method = "svmRadial", trControl = fitControl,verbose = FALSE)
svm_p$finalModel
plot(svm_p)

pred <- predict(svm_p,test)
t <- table(pred,test$i_next_M)
confusionMatrix(t)


Shibors$profit = 0

for (i in 2:nrow(Shibors)){
  Shibors$profit[i] = Shibors$profit[i-1] -Shibors$profit[i]  
}
testProfit <- Shibors[ind==2,]
testProfit 
testProfit <- testProfit [1:10,]
test_p = test [1:10,]
testProfit <- cbind(testProfit,test_p)
testProfit <- cbind(testProfit[8],testProfit[15])
testProfit

testProfit$A_P =0
for (i in 1:nrow(testProfit)){
  if (testProfit$i_next_M[i]=="up" & testProfit$Profit[i]<0){
    testProfit$A_P[i] = abs (testProfit$Profit[i])
  }else if(testProfit$i_next_M[i]=="down" & testProfit$Profit[i]>0){
    testProfit$A_P[i] = abs (testProfit$Profit[i])
  }else{
    testProfit$A_P[i] = - abs (testProfit$Profit[i])
  }
}
testProfit
overallProfit = sum(testProfit[3])
overallProfit