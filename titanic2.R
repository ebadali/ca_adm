titanicData <- read.csv("titanic.csv", header = T, na.strings = c(""), stringsAsFactors = T)
unique(titanicData$Survived)

titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)

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
(titanicData$Fsize)



# Logistic Regression
set.seed(1337)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .80, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]

logit <- glm(Survived~., family = binomial(link = "logit"), data = training)
summary(logit)

logit.prediction <- predict(logit, newdata = testing, type = "response")
results.logit <- ifelse(logit.prediction > 0.5,"Yes","No")

results.logit

# table confusion matrix
table(testing$Survived, logit.prediction > 0.5)


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
titanicData <- titanicData[-8]

set.seed(1337)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .80, replace=FALSE)

training <- titanicData[index, ]
testing <- titanicData[-index, ]


# Run kNN
library("class")
library("caret")


head(training)

sum(is.na(testing))
# Evaluate the change in accuracy with increasing values of k
KnnTestPrediction <- list()
accuracy <- numeric()

for(k in 1:100)
{
  KnnTestPrediction[[k]] <- knn(training, testing, training$Survived[-4], k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testing$Survived)/length(testing$Survived)*100
}

accuracy




