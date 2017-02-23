# Trevor Stephens Titantic Kaggle tutoral

setwd("~/Documents/kaggle/Titanic4/TrevorStephensTutorial")

# load train.csv
library(readr)
train <- read_csv("~/Documents/kaggle/Titanic4/train.csv")
test <- read_csv("~/Documents/kaggle/Titanic4/test.csv")
#View(train)

# Submission 1 - everyone dies
test$Survived <- rep(0, 418)
submit <- data.frame(test[c('PassengerId', 'Survived')])
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Submission 2 - females survive
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(test[c('PassengerId', 'Survived')])
write.csv(submit, file = "femalesSurvive.csv", row.names = FALSE)

# Submission 3 - females except 3rd class, $20+ tickets survive
train$Child <- 0
train$Child[train$Age < 18] <- 1
test$Child <- 0
test$Child[test$Age < 18] <- 1
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(test[c('PassengerId', 'Survived')])
write.csv(submit, file = "femalesSurvive_except3rd.csv", row.names = FALSE)

# Submission 4 - Random Trees
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Submission 5 - Overfit Random Tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class", 
               control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Feature Engineering..
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

# Random Forests
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
combi$Embarked[which(is.na(combi$Embarked))] = 'S'
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm = TRUE)

library(randomForest)

set.seed(1984)
train <- combi[1:891,]
test <- combi[892:1309,]
train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             importance=TRUE,
             ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Conditional inference trees
library(party)

set.seed(1984)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest.csv", row.names = FALSE)

# What's next..
# FamilyID and Size
# Ticket/Cabin numbers
