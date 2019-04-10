library(mice)
library(randomForest)

titanicData <- read.csv("titanic.csv", header = T, na.strings = c(""), stringsAsFactors = T)
head(titanicData)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)


# Check missing value in a frame
sapply(titanicData, function(x) sum(is.na(x)))

# fix missing values

summary(titanicData$Age)

titanicData$Age <- (replace(titanicData$Age, is.na(titanicData$Age), 1))
summary(titanicData$Age)

# titanicData$Age <- titanicData%age[,] 
titanicData$Cabin
typeof(titanicData$Cabin)
mice_mod <- mice(titanicData, method='rf')
mice_mod





#load and normalise ris
irisData <- iris

head(irisData)

