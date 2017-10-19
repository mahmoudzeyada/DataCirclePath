<<<<<<< HEAD
---
title: "Titanic EDA"
author: "Omar Hesham"

---


#$Agenda$


1.Introduction
    
*1.1 Loading Data 
   
*1.2 Summariznig it

2.Removing NAs

3.New Features?

*3.1 Nobles ? 

*3.2 Travelling Alone ?

4. Making Prediction Using Decision Tree


 
##$Introduction$

Welcome To My First EDA Where I Will Do Some Basic Minuplation And Prediction With Titanic DataSet But First Let's Load Some Packages Which Might Help us to do so

```{r message=FALSE,warning=FALSE}

library(rpart) #Decision Tree
library(dplyr) #Data Frame Minuplation
library(ggplot2) #Plotting
library(data.table)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


```



###Loading Data

We Have 2 DataSets,Train DataSet Which has the Correct Predictions And The Test Which We Should Apply our model on it So Let's Load Them And See The Variables 
```{r}
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)
```
###Summarizing it
Before Making any Work On the data We Must Make Sure it's Clean And Well-Organized So Let's Look At The Both DataSets
```{r}
summary(train)
summary(test)
```
Mmmmm There Are Some NAs at Some Columns So We Must Remove it So Instead Of Working On Both DataSets Let's Combine Them To Optmize Our Code.
First We Must Make Sure that They Have The Same Columns Number.
```{r}
nrow(train)==nrow(test)
```
The Missing Column is the "Survived" in the test data So We Put it And Assign it to NA Temporary , Add A Feature That Help Us to Seperate The 2 DataSets After We Finish Cleaning And rbind them after that.
```{r}
test$Survived<-NA
train$IsTrain<-TRUE
test$IsTrain<-FALSE
full<-rbind(train,test)
```
So Let's Look at The Full DataSet
```{r}
str(full)
```


We've got a sense of our variables, their class type, and the first few observations of each. We know we're working with 1309 observations of 13 variables. To make things a bit more explicit since a couple of the variable names aren't 100% illuminating, here's what we've got to deal with:

Variable Name | Description
--------------|-------------
Survived      | Survived (1) or died (0)
Pclass        | Passenger's class
Name          | Passenger's name
Sex           | Passenger's sex
Age           | Passenger's age
SibSp         | Number of siblings/spouses aboard
Parch         | Number of parents/children aboard
Ticket        | Ticket number
Fare          | Fare
Cabin         | Cabin
Embarked      | Port of embarkation
IsTrain       | Define The Train DS
-----------------------------------
Let's Look At The Train DS And See What We've Got
```{r }
barplot(table(train$Survived,train$Sex),
        sub="Survival By Sex",
        ylab="Number Of Passengers",
      col=c("Red","Green"))
legend("topleft",legend = c("Died","Survived"),fill = c("Red","Green"),inset = 0.05)

```
--------------------------------------------

It's Obvious That The Female Survived More Than Men Which Gives Us an info when We Make the Predictive Model
Let's Make Another Assumption That The Women And Children Survived More Than Men So Let's Check it Out
```{r}


temp.data<-train[!is.na(train$Age),]
temp.data$Child<-temp.data$Age<16
barplot(prop.table(table(temp.data$Survived,temp.data$Child),2),
        sub = "Survival By Children",
        ylab = "Proportion",
        col = c("Red","Green"))
legend("topright",legend=c("Died","Survived"),fil=c("Red","Green"),inset=0.05)
        

```


So Our Assumption Is True So Let's Leave that For Now and Clean Our Data.

------------------------------------------

##$Removing NAs$

Let's Look Again At Our Variables.

```{r echo=FALSE}
summary(full)

```

As We See The Variables(Fare,Age) Have NAs in Them So Let's Start With Age.
I'll do something very basic but it isn't that accurate, I'll Fill The NAs With The Median Of The Age and do the same thing with the Fare So Let's Do This

```{r}
agemedian<-median(full$Age,na.rm = TRUE)
faremedian<-median(full$Fare,na.rm = TRUE)
full[is.na(full$Age),"Age"]<-agemedian
full[is.na(full$Fare),"Fare"]<-faremedian
```

And From Seeing Other EDAs I saw that the emarked has a missing value too

```{r}
table(full$Embarked)
```

It's True! There are 2 Values So Let's Correct Them.

```{r}
full[full$Embarked=='',"Embarked"]<- 'S'

```

Let's Look After The Cleaning.

```{r echo=FALSE}
summary(full)
```

Sounds Good To Continue To Our Next Step.

------------------------------------------
##$New  Features$

###Nobles ?

A Good Feature We Can Add that we Can See The relation between the Survival and the nobles that they were on the ship so let's Seperate Them And Combine the rare titles as well.

```{r}
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title)
full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(full$Title)
```

###Travelling Alone

Many EDAs have pointed out that the variables Parch and Sibsp contain hidden information about a passenger’s family. SibSp tells us how many siblings or spouses are travelling onboard and Parch tells us how many parents or children are onboard. We can use this information to tell how many people the passenger was travelling with, which might have an impact on whether they survived or not.

```{r}
full$Fsize <- full$SibSp + full$Parch + 1

```

After That We Convert The Variables We Need to Factors To Complete Prediction

```{r}
full$Survived<-as.factor(full$Survived)
full$Sex<-as.factor(full$Sex)
full$Pclass<-as.factor(full$Pclass)
full$Embarked<-as.factor(full$Embarked)
full$Title<-as.factor(full$Title)
```

##Make Prediction Using Decision Tree

Let's Seperate the 2 DS So We can Do Our Prediction 

```{r}
train<-full[full$IsTrain==TRUE,]
test<-full[full$IsTrain==FALSE,]
```

I'm Gonna Use Decision Trees Because That's What I Slightly Understood

```{r}
modelfit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize,data=train,
               method="class")
fancyRpartPlot(modelfit)
```

So Let's Apply To Test

```{r}
Survived<- predict(modelfit,test,type="class")
test$Survived<-Survived

plot(test$Survived)
```




















 
=======
---
title: "Titanic EDA"
author: "Omar Hesham"

---


#$Agenda$


1.Introduction
    
*1.1 Loading Data 
   
*1.2 Summariznig it

2.Removing NAs

3.New Features?

*3.1 Nobles ? 

*3.2 Travelling Alone ?

4. Making Prediction Using Decision Tree


 
##$Introduction$

Welcome To My First EDA Where I Will Do Some Basic Minuplation And Prediction With Titanic DataSet But First Let's Load Some Packages Which Might Help us to do so

```{r message=FALSE,warning=FALSE}

library(rpart) #Decision Tree
library(dplyr) #Data Frame Minuplation
library(ggplot2) #Plotting
library(data.table)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


```



###Loading Data

We Have 2 DataSets,Train DataSet Which has the Correct Predictions And The Test Which We Should Apply our model on it So Let's Load Them And See The Variables 
```{r}
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)
```
###Summarizing it
Before Making any Work On the data We Must Make Sure it's Clean And Well-Organized So Let's Look At The Both DataSets
```{r}
summary(train)
summary(test)
```
Mmmmm There Are Some NAs at Some Columns So We Must Remove it So Instead Of Working On Both DataSets Let's Combine Them To Optmize Our Code.
First We Must Make Sure that They Have The Same Columns Number.
```{r}
nrow(train)==nrow(test)
```
The Missing Column is the "Survived" in the test data So We Put it And Assign it to NA Temporary , Add A Feature That Help Us to Seperate The 2 DataSets After We Finish Cleaning And rbind them after that.
```{r}
test$Survived<-NA
train$IsTrain<-TRUE
test$IsTrain<-FALSE
full<-rbind(train,test)
```
So Let's Look at The Full DataSet
```{r}
str(full)
```


We've got a sense of our variables, their class type, and the first few observations of each. We know we're working with 1309 observations of 13 variables. To make things a bit more explicit since a couple of the variable names aren't 100% illuminating, here's what we've got to deal with:

Variable Name | Description
--------------|-------------
Survived      | Survived (1) or died (0)
Pclass        | Passenger's class
Name          | Passenger's name
Sex           | Passenger's sex
Age           | Passenger's age
SibSp         | Number of siblings/spouses aboard
Parch         | Number of parents/children aboard
Ticket        | Ticket number
Fare          | Fare
Cabin         | Cabin
Embarked      | Port of embarkation
IsTrain       | Define The Train DS
-----------------------------------
Let's Look At The Train DS And See What We've Got
```{r }
barplot(table(train$Survived,train$Sex),
        sub="Survival By Sex",
        ylab="Number Of Passengers",
      col=c("Red","Green"))
legend("topleft",legend = c("Died","Survived"),fill = c("Red","Green"),inset = 0.05)

```
--------------------------------------------

It's Obvious That The Female Survived More Than Men Which Gives Us an info when We Make the Predictive Model
Let's Make Another Assumption That The Women And Children Survived More Than Men So Let's Check it Out
```{r}


temp.data<-train[!is.na(train$Age),]
temp.data$Child<-temp.data$Age<16
barplot(prop.table(table(temp.data$Survived,temp.data$Child),2),
        sub = "Survival By Children",
        ylab = "Proportion",
        col = c("Red","Green"))
legend("topright",legend=c("Died","Survived"),fil=c("Red","Green"),inset=0.05)
        

```


So Our Assumption Is True So Let's Leave that For Now and Clean Our Data.

------------------------------------------

##$Removing NAs$

Let's Look Again At Our Variables.

```{r echo=FALSE}
summary(full)

```

As We See The Variables(Fare,Age) Have NAs in Them So Let's Start With Age.
I'll do something very basic but it isn't that accurate, I'll Fill The NAs With The Median Of The Age and do the same thing with the Fare So Let's Do This

```{r}
agemedian<-median(full$Age,na.rm = TRUE)
faremedian<-median(full$Fare,na.rm = TRUE)
full[is.na(full$Age),"Age"]<-agemedian
full[is.na(full$Fare),"Fare"]<-faremedian
```

And From Seeing Other EDAs I saw that the emarked has a missing value too

```{r}
table(full$Embarked)
```

It's True! There are 2 Values So Let's Correct Them.

```{r}
full[full$Embarked=='',"Embarked"]<- 'S'

```

Let's Look After The Cleaning.

```{r echo=FALSE}
summary(full)
```

Sounds Good To Continue To Our Next Step.

------------------------------------------
##$New  Features$

###Nobles ?

A Good Feature We Can Add that we Can See The relation between the Survival and the nobles that they were on the ship so let's Seperate Them And Combine the rare titles as well.

```{r}
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title)
full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(full$Title)
```

###Travelling Alone

Many EDAs have pointed out that the variables Parch and Sibsp contain hidden information about a passenger’s family. SibSp tells us how many siblings or spouses are travelling onboard and Parch tells us how many parents or children are onboard. We can use this information to tell how many people the passenger was travelling with, which might have an impact on whether they survived or not.

```{r}
full$Fsize <- full$SibSp + full$Parch + 1

```

After That We Convert The Variables We Need to Factors To Complete Prediction

```{r}
full$Survived<-as.factor(full$Survived)
full$Sex<-as.factor(full$Sex)
full$Pclass<-as.factor(full$Pclass)
full$Embarked<-as.factor(full$Embarked)
full$Title<-as.factor(full$Title)
```

##Make Prediction Using Decision Tree

Let's Seperate the 2 DS So We can Do Our Prediction 

```{r}
train<-full[full$IsTrain==TRUE,]
test<-full[full$IsTrain==FALSE,]
```

I'm Gonna Use Decision Trees Because That's What I Slightly Understood

```{r}
modelfit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize,data=train,
               method="class")
fancyRpartPlot(modelfit)
```

So Let's Apply To Test

```{r}
Survived<- predict(modelfit,test,type="class")
test$Survived<-Survived

plot(test$Survived)
```




















 
>>>>>>> e6dc149c6917b7cceaca69aa8bd060a6b6eb9324
   