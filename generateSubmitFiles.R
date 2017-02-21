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



