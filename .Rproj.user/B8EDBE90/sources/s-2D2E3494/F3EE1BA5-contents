# Model Evaluation - kNN

#load and normalise ris
irisData <- iris

# Normalise all columns except species
irisData[, -5] <- scale(iris[, -5])
head(irisData)

# 70/30 split into training and test sets
set.seed(17160979)
index <- sample(1:nrow(irisData), nrow(irisData) * .70, replace=FALSE)

training <- irisData[index, ]
test <- irisData[-index, ]


# Run kNN
library("class")
library("caret")

# k = 1
kNNPred1 <- knn(training[,-5], test[,-5], training$Species, k=1, prob=T)

# confusion matrix
table(test$Species, kNNPred1)

# calculate overall accuracy
sum(kNNPred1 == test$Species)/length(test$Species)*100


# k = 2
kNNPred2 <- knn(training[,-5], test[,-5], training$Species, k=2, prob=T)

# confusion matrix
table(test$Species, kNNPred2)

# calculate overall accuracy
sum(kNNPred2 == test$Species)/length(test$Species)*100


# k = 3
kNNPred3 <- knn(training[,-5], test[,-5], training$Species, k=3, prob=T)

# confusion matrix
table(test$Species, kNNPred3)

# calculate overall accuracy percentage
sum(kNNPred3 == test$Species)/length(test$Species)*100


# Easier confusion matrix and statistics using the "confusionMatrix" function
# from the "caret" packages
confusionMatrix(kNNPred3, test$Species)


# Evaluate the change in accuracy with increasing values of k
KnnTestPrediction <- list()
accuracy <- numeric()

for(k in 1:100)
{
  KnnTestPrediction[[k]] <- knn(training[,-5], test[,-5], training$Species, k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==test$Species)/length(test$Species)*100
}

plot(accuracy, type="b", col="blue", cex=1, pch=20, 
     xlab="Number of neighbours (k)", ylab="Classification Accuracy (%)", 
     main="Accuracy vs k")


# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)
paste("Maximum accuracy is", max(accuracy), "% at k = ", which(accuracy==max(accuracy)))


# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)


# Advanced

# Cleaning and pre-processing
######################################################################################################
library(mice)
library(randomForest)

titanicData <- read.csv("titanic.csv", header = T, na.strings = c(""), stringsAsFactors = T)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)

titanicData <- titanicData[, -c(1,11)]

titanicData$Embarked[c(62, 830)] <- 'C'

#use a random forest to impute missing age values
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)
#feature engineer a few more things using the passenger name
titanicData$Name <- as.character(titanicData$Name)
titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1
#remove features 3, 7, and 11

titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
# feature engineer a family size categorical variable
titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)

write.csv(titanicData, "titanicCleaned.csv", row.names = FALSE)
######################################################################################################



# Logistic Regression
set.seed(1337)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .80, replace=FALSE)

training <- titanicData[index, ]
testing <- titanicData[-index, ]

logit <- glm(Survived~., family = binomial(link = "logit"), data = training)
summary(logit)

# Pclass, Title and to some extent family size are significant


# prediction
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
