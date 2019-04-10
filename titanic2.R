
install.packages("magrittr") # only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("data.table")
install.packages("Amelia")
library("mice")
titanicData <- read.csv("titanic.csv", header = T, na.strings = c(""), stringsAsFactors = T)
unique(titanicData$Survived)


titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))


library(C50)
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
summary(testing[,-1])
summary(testing)
cFifty <- C5.0(Survived ~ ., data=training)
c <- predict(cFifty, testing)
caret::confusionMatrix(c, testing$Survived, positive="Yes")

str(titanicData)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)

spineplot(titanicData$Age,titanicData$Survived) 


unique(titanicData$Pclass)

head(titanicData)
titanicData <- titanicData[, -c(1,11)]

head(titanicData)

sapply(titanicData, function(x) sum(is.na(x)))



which(is.na(titanicData$Embarked))

titanicData$Embarked[c(62, 830)] <- 'C'

#use a random forest to impute missing age values
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age



titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
head(titanicData)
# titanicData[1] <- NULL
summary(titanicData)


titanicData$Survived <- factor(titanicData$Survived, labels=c(0,1), levels=c("No", "Yes"))

#F2: class balance:
table(titanicData$Survived) #fairly imbalanced


# Logistic Regression
set.seed(1337)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .80, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]




str(testing$Survived) # remain (Y) is 0
b1 <- rep(0, dim(testing)[1])
b1
(accuracyB1 <- 1 - mean(b1 != testing$Survived))

logit <- glm(Survived~., family = binomial(link = "logit"), data = training)
summary(logit)

logit.prediction <- predict(logit, newdata = testing, type = "response")
results.logit <- ifelse(logit.prediction > 0.5,"Yes","No")

results.logit

# table confusion matrix
table(testing$Survived, logit.prediction > 0.5)

library(gmodels)
CrossTable(testing$Survived, results.logit, prop.chisq = F, prop.c = F, prop.r = F)

# ROC and AUC for logistic prediction
library(ROCR)
ROCRpred <- prediction(logit.prediction, testing$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


################## KNN ##################
sapply(titanicData, function(x) sum(is.na(x)))
titanicData <- titanicData[-4]

set.seed(1337)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .80, replace=FALSE)

training <- titanicData[index, ]
testing <- titanicData[-index, ]


# Run kNN
library("class")
library("caret")

levels(training$Age)
head(training)

sum(is.na(testing))
# Evaluate the change in accuracy with increasing values of k
KnnTestPrediction <- list()
accuracy <- numeric()
varValue <- c(2)
for(k in 1:10)
{
  KnnTestPrediction[[k]] <- knn(training, testing, training$Survived, k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testing$Survived)/length(testing$Survived)*100
}

accuracy


