

# Doing it all

install.packages("magrittr") # only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("data.table")
install.packages("Amelia")

######################  Loading and basic cleaning  ######################

titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
head(titanicData)

library(Amelia)
#Visual representation of missing data 
missmap(titanicData, main = "Missing values vs observed")

head(titanicData)

library(mice)


integerColumnsTitanic <- colnames(titanicData[, sapply(titanicData, class) != "numeric"])
head(integerColumnsTitanic)

#mice_mod <- mice(titanicData[, !names(titanicData) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')

mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               integerColumnsTitanic], method='rf')

mice_output <- complete(mice_mod)
summary(mice_output)
head(mice_mod)
head(titanicData)
titanicData$Age <- mice_output$Age
titanicData$Fare <- mice_output$Fare
missmap(titanicData, main = "Missing values vs observed")
head(titanicData)
# Do for the cabin too !
mice_mod <- mice(titanicData[, names(titanicData) %in%
                               c("cabin","Embarked")], method='rf')
colnames(-titanicData[9])

library(plyr)

count(is.nan(titanicData$Cabin))
count(titanicData,"Cabin")
####################### KNN Algorithms #######################


tot <- count(is.na(titanicData$Cabin))
tot$freq[1] * 100 / (tot$freq[1]+tot$freq[2])

titanicData$Cabin <- NULL
head(titanicData)

titanicData <- na.omit(titanicData)


sum(is.na(titanicData))

####### 1. KNN
# Run kNN
library("class")
library("caret")


titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)


head(titanicData)
set.seed(666)
index <- sample(1:nrow(titanicData), nrow(titanicData) * .70, replace=FALSE)

training <- titanicData[index, ]
test <- titanicData[-index, ]


# Evaluate the change in accuracy with increasing values of k
KnnTestPrediction <- list()
accuracy <- numeric()


for(k in 1:100)
{
  KnnTestPrediction[[k]] <- knn(training[,-1], test[,-1], training$Survived, k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==test$Survived)/length(test$Survived)*100
}

