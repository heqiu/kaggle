setwd("~/Dropbox/Kaggle/Titanic")
train <- read.csv("~/Dropbox/Kaggle/Titanic/train.csv")
test = read.csv("~/Dropbox/Kaggle/Titanic/test.csv")

#gives a summary of train
str(train)

# gives survived column of all the passengers.  
# $ gives you that particular column in the table
numSurvived = table(train$Survived)

# gives proportion of dead
prop.table(numSurvived)

#put all surive for test as false
#we are predicting everyone died, our most basic model
test$Survived = rep(0, dim(test)[1])

#makes a submit file.  kaggle wants ID + Surived
submit = data.frame(PassengerID = test$PassengerId, Survived = test$Survived)

#row.names prevents overriding of index, so it starts at 892 instead of 0
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


#GENDER CLASS MODEL: PART DOS
summary(train$Sex)

#each comma indicates a whole other dimension
prop.table(table(train$Sex, train$Survived))

# number of females survived/ number of females.  
# the 1 indicates the dimension to sum across for the denominator
prop.table(table(train$Sex, train$Survived), 1)

test$Survived[test$Sex == 'male'] = 0
test$Survived[test$Sex != 'male'] = 1

#makes a submit file.  kaggle wants ID + Surived
submit = data.frame(PassengerID = test$PassengerId, Survived = test$Survived)

#row.names prevents overriding of index, so it starts at 892 instead of 0
write.csv(submit, file = "gender.csv", row.names = FALSE)


#include whether or not it's a child
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

#note the usage of a single "&" sign
table(train$Survived[train$Sex == 'female' & train$Fare > 30])