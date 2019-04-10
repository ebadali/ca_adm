# Cleaning the data:

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



# Fixing missing vallues

# 1. Removing the missing rows
# titanicData <- titanicData[!is.na(titanicData$Embarked), ]

# 2. or find out which one are those ?
library(dplyr)
# %>% is a basically a pipe
#embarked <- titanicData %>% filter(PassengerId != 62 & PassengerId != 830)
embarked <- filter(titanicData$PassengerId, rep(62,830) )
embarked
# Actually replacing values

data <- titanicData$Age
mean(data, na.rm = TRUE)
is.na(data)

data[is.na(data)] <- mean(data,na.rm = TRUE)
summary(data)

titanicData$Age <- data

# Or replacing by imputation

library(mice)
# Perform mice imputation, excluding some variables that probably won't help: 
# mice_mod <- mice(titanicData[, ], method='rf')

# Getting all integer classes
integerColumnsTitanic <- colnames(titanicData[, sapply(titanicData, class) != "numeric"])
head(integerColumnsTitanic)

(integerColumnsTitanic)
c(integerColumnsTitanic)
#mice_mod <- mice(titanicData[, !names(titanicData) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')

mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               integerColumnsTitanic], method='rf')

summary(mice_mod)
# Complete the missing values
mice_output <- complete(mice_mod)
summary(mice_output)
titanicData$Age <- mice_output$Age
titanicData$Fare <- mice_output$Fare


summary(titanicData$Fare)
titanicData$Cabin <- mice_output$Cabin

summary(titanicData$Cabin)

######################  Visualization and Exploration  ######################

# since the target variable is 'Survied'
# Lets start by plotting the number of survived with the unservived

ggplot2::ggplot(titanicData, ggplot2::aes(x = Sex, fill = factor(Survived))) + ggplot2::geom_bar(stat='count', position='dodge') + 
  ggplot2::labs(x = 'Sex') + ggplot2::theme()


table(titanicData$Sex, titanicData$Age)


dfc <- cut(titanicData$Age, breaks=c(0, 15, 45, 56, Inf))


ageFactors <- factor(titanicData$Age, levels = c(20,60,80))

barchart(table(dfc,titanicData$Sex))
  
######################  Checking imbalance of datasets  ######################

# Whether one class outnumbers other class ? eg: more frequently survived not survvide




######################  Checking Coorelation  ######################
head(mice_output)
cov(mice_output,use="complete.obs", type = c("pearson","spearman"))


library(corrplot)
corrplot(corrplot)
corrplot(mice_output, type = "upper", order = "hclust", 
         tl.col = "black")

CrossTable(x = mice_output$Age, y=mice_output$Fare )
install.packages("psych")
library(psych)
 
pairs.panels(titanicData[c("Age","Fare","Survived","Pclass","Name","Sex","SibSp" )])


library(GGally)
ggcorr(titanicData,label = TRUE)

