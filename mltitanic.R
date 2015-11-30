

#Machine learning
#written by:Abinah
#last modified:2/9/2015
setwd("E:/datajam")
#load packages
library(Amelia)
library(stringr)
library(vcd)
library(party)
library(caret)
#get data,i got my data from kaggle4`   1   ``  A2
titanic=read.csv("train.csv",na.strings = c("NA",""))
str(titanic)
#impute using Amalia Gui and load again
titanic=read.csv("train-imp1.csv",na.strings = c("NA",""))
# some variables are lebbled as integers while they seam to be factors i have to convert them
#to factors
titanic$Survived=factor(titanic$Survived)
titanic$Pclass=factor(titanic$Pclass)
titanic$SibSp=factor(titanic$SibSp)
titanic$Parch=factor(titanic$Parch)
#detecting missing values
#write a function that finds the missing values and calculates the percentage
x=function(df){
    sum(is.na(df))/length(df)
}
sapply(titanic,x)  #we can also use lapply
#plotting the missing values using the Amelia package
missmap(titanic,main="MISSING VALUES")
#imputing missing values
table(titanic$Embarked, useNA = "always")
#assign the two missing values to one,ie S
titanic$Embarked[which(is.na(titanic$Embarked))] = 'S';
table(titanic$Embarked, useNA = "always")
#imputing missing Age
#discover titles in the Names
titanic$Name = as.character(titanic$Name)
table_words = table(unlist(strsplit(titanic$Name, "\\s+")))
sort(table_words [grep('\\.',names(table_words))], decreasing=TRUE)
#find missing values per title
tb = cbind(titanic$Age, str_match(titanic$Name, " [a-zAZ]+\\."))
table(tb[is.na(tb[,1]),2])
#impute by assigning the mean of that category,so fast we calculate the means
mean.mr = mean(titanic$Age[grepl(" Mr\\.", titanic$Name) &
                               !is.na(titanic$Age)])
mean.mrs = mean(titanic$Age[grepl(" Mrs\\.", titanic$Name) &
                                !is.na(titanic$Age)])
mean.dr = mean(titanic$Age[grepl(" Dr\\.", titanic$Name) &
                               !is.na(titanic$Age)])
mean.miss = mean(titanic$Age[grepl(" Miss\\.", titanic$Name) &
                                 !is.na(titanic$Age)])
mean.master = mean(titanic$Age[grepl(" Master\\.",
                                     titanic$Name) & !is.na(titanic$Age)])
#then assign the mean to the missing values
titanic$Age[grepl(" Mr\\.", titanic$Name) &
                is.na(titanic$Age)] = mean.mr
titanic$Age[grepl(" Mrs\\.", titanic$Name) &
                is.na(titanic$Age)] = mean.mrs
titanic$Age[grepl(" Dr\\.", titanic$Name) &
                is.na(titanic$Age)] = mean.dr
titanic$Age[grepl(" Miss\\.", titanic$Name) &
                is.na(titanic$Age)] = mean.miss
titanic$Age[grepl(" Master\\.", titanic$Name) &
                is.na(titanic$Age)] = mean.master

#data visualisation

#bar plot
barplot(table(titanic$Survived),main="perssangers who survived",names=c("dead","survived"),col=c(6,5))
#plot survival and sex
counts = table( titanic$Survived, titanic$Sex)
barplot(counts, col=c("darkblue","red"), legend = c("Perished",
                                                    "Survived"), main = "Passenger Survival by Sex")

#does class affect survival?
counts = table( titanic$Survived, titanic$Pclass)
barplot(counts, col=c("darkblue","red"), legend =c("Perished",
                                                   "Survived"), main= "Titanic Class Bar Plot" )

#composition of class by gender
counts = table( titanic$Sex, titanic$Pclass)
barplot(counts, col=c("darkblue","red"), legend = rownames(counts),
        main= "Passenger Gender by Class")

#histogram of pass ages
hist(titanic$Age[which(titanic$Survived == "0")], main=
         "Passenger Age Histogram", xlab="Age", ylab="Count", col ="blue",
     breaks=seq(0,80,by=2))
hist(titanic$Age[which(titanic$Survived == "1")], col ="red",
     add = T, breaks=seq(0,80,by=2))
#boxplot of age and survival
boxplot(titanic$Age ~ titanic$Survived,
        main="Passenger Survival by Age",
        xlab="Survived", ylab="Age")

#categorising data into groups according to age

children = titanic$Survived[titanic$Age < 13]
length(children[which(children == 1)] ) / length(children)
youths = titanic$Survived[titanic$Age >= 15 &
                              titanic$Age < 25]
length(youths[which(youths == 1)] ) / length(youths)
adults = titanic$Survived[titanic$Age >= 20 &
                              titanic$Age < 65]
length(adults[which(adults == 1)] ) / length(adults)
seniors = titanic$Survived[titanic$Age >= 65]
length(seniors[which(seniors == 1)] ) /
    length(seniors)

#mosaic plot
mosaicplot(titanic$Pclass ~ titanic$Survived,
           main="Passenger Survival Class", color=TRUE,
           xlab="Pclass", ylab="Survived")

#decision tree:predicting passanger survival
split.data = function(data, p = 0.7, s = 666){
    set.seed(s)
    index = sample(1:dim(data)[1])
    train = data[index[1:floor(dim(data)[1] * p)], ]
    test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)
                      [1]], ]
    return(list(train = train, test = test))
}

allset= split.data(titanic, p = 0.7)
trainset = allset$train
testset = allset$test


titanic.ctree = ctree(Survived ~ Pclass + Sex + Age + SibSp + Fare +
                          Parch + Embarked, data=trainset)
titanic.ctree

plot(titanic.ctree, main="Conditional inference tree of Titanic
     Dataset")
#assesing the  prediction model
#1.use the titanic.ctreee model to try predict the testing set
ctree.predict = predict(titanic.ctree, testset)
#2.Using caretuse a confusion matrix to generate the statistics of the output matrix:
confusionMatrix(ctree.predict, testset$Survived)

