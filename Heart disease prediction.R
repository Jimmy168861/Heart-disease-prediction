###Min Jiang_Zheng_Li_Yundi Lu_Xun Pan
###Final project
###BU510.650 - Data Analytics, Spring 2019

library(MASS)
library(ISLR)
library(plyr)
library(tree)
library(leaps)
library(class)

#download the data from the online source
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data)<-c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg","thalach","exang", "oldpeak","slope", "ca", "thal", "num")
View(heart.data)

#remove the missing value
dim(heart.data)
sum(is.na(heart.data))

heart.data <- na.omit(heart.data)
sum(is.na(heart.data))
dim(heart.data)

#Univariate Analysis
table(heart.data$age)
summary(heart.data$age)
var(heart.data$age)
heart.data.exploration<-heart.data
heart.data.exploration$age[heart.data$age < 30 & heart.data$age >= 20]="20-30"
heart.data.exploration$age[heart.data$age < 40 & heart.data$age >= 30]="30-40"
heart.data.exploration$age[heart.data$age < 50 & heart.data$age >= 40]="40-50"
heart.data.exploration$age[heart.data$age < 60 & heart.data$age >= 50]="50-60" 
heart.data.exploration$age[heart.data$age < 70 & heart.data$age >= 60]="60-70"
heart.data.exploration$age[heart.data$age < 80 & heart.data$age >= 70]="70-80"
heart.data.exploration$dis=0
heart.data.exploration$dis[heart.data$num>=1]=1
table(heart.data.exploration$sex, heart.data.exploration$age)
table(heart.data.exploration$dis, heart.data.exploration$sex)

#Bi-variate Analysis
#Continuous & Continuous:
pairs(~ age+trestbps+chol+thalach+oldpeak, heart.data)

#Categorical & Categorical
heart.data.categorical=table(heart.data[,2], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,7], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,9], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,12], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,13], heart.data[,14])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,3])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,6])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,7])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,9])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,11])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,2], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,6])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,7])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,9])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,11])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,3], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,6], heart.data[,7])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,6], heart.data[,9])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,6], heart.data[,11])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,6], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,6], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,7], heart.data[,9])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,7], heart.data[,11])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,7], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,7], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,9], heart.data[,11])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,9], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,9], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,11], heart.data[,12])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,11], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

heart.data.categorical=table(heart.data[,12], heart.data[,13])
chisq.test(heart.data.categorical)
plot(heart.data.categorical)

#Categorical & Continuous:
anova = aov(heart.data[,1]~heart.data[,14], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,14], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,14], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,14], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,14], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,2], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,3], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,6], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,7], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,9], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,11], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,12], data=heart.data)
summary(anova)

anova = aov(heart.data[,1]~heart.data[,13], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,2], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,3], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,6], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,7], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,9], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,11], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,12], data=heart.data)
summary(anova)

anova = aov(heart.data[,4]~heart.data[,13], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,2], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,3], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,6], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,7], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,9], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,11], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,12], data=heart.data)
summary(anova)

anova = aov(heart.data[,5]~heart.data[,13], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,2], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,3], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,6], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,7], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,9], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,11], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,12], data=heart.data)
summary(anova)

anova = aov(heart.data[,8]~heart.data[,13], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,2], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,3], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,6], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,7], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,9], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,11], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,12], data=heart.data)
summary(anova)

anova = aov(heart.data[,10]~heart.data[,13], data=heart.data)
summary(anova)

#remove outliers

par(mfrow = c(1, 1))
boxplot(heart.data[,1])
boxplot(heart.data[,1], plot=FALSE)$out


par(mfrow = c(1, 2))
boxplot(heart.data[,4])
boxplot(heart.data[,4], plot=FALSE)$out
outliers <- boxplot(heart.data[,4], plot=FALSE)$out
heart.data[which(heart.data[,4] %in% outliers),]
heart.data <- heart.data[-which(heart.data[,4] %in% outliers),]
boxplot(heart.data[,4])


par(mfrow = c(1, 2))
boxplot(heart.data[,5])
boxplot(heart.data[,5], plot=FALSE)$out
outliers <- boxplot(heart.data[,5], plot=FALSE)$out
heart.data[which(heart.data[,5] %in% outliers),]
heart.data <- heart.data[-which(heart.data[,5] %in% outliers),]
boxplot(heart.data[,5])


par(mfrow = c(1, 2))
boxplot(heart.data[,8])
boxplot(heart.data[,8], plot=FALSE)$out
outliers <- boxplot(heart.data[,8], plot=FALSE)$out
heart.data[which(heart.data[,8] %in% outliers),]
heart.data <- heart.data[-which(heart.data[,8] %in% outliers),]
boxplot(heart.data[,8])



par(mfrow = c(1, 2))
boxplot(heart.data[,10])
boxplot(heart.data[,10], plot=FALSE)$out
outliers <- boxplot(heart.data[,10], plot=FALSE)$out
heart.data[which(heart.data[,10] %in% outliers),]
heart.data <- heart.data[-which(heart.data[,10] %in% outliers),]
boxplot(heart.data[,10])

dim(heart.data)

heart.data.final=heart.data



#logistic regression
heart.data$num[heart.data$num > 0] <- 1
par(mfrow = c(1, 1))


count(heart.data,14)
barplot(table(heart.data$num),main="Fate", col="black")
heart.data$num<-as.factor(heart.data$num)

heart.data.8<-heart.data

heart.data.8[,2] <- factor(heart.data.8[,2])
heart.data.8[,3] <- factor(heart.data.8[,3])
heart.data.8[,6] <- factor(heart.data.8[,6])
heart.data.8[,7] <- factor(heart.data.8[,7])
heart.data.8[,9] <- factor(heart.data.8[,9])
heart.data.8[,11] <- factor(heart.data.8[,11])
heart.data.8[,12] <- factor(heart.data.8[,12])
heart.data.8[,13] <- factor(heart.data.8[,13])
heart.data.8[,14] <- factor(heart.data.8[,14])


logreg.fit <- glm(num~.,data=heart.data.8,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(num~.-age,data=heart.data.8,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(num~.-age-restecg,data=heart.data.8,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(num~.-age-restecg-thalach,data=heart.data.8,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(num~.-age-restecg-thalach-fbs,data=heart.data.8,family=binomial)
summary(logreg.fit)

#relative best choice
logreg.fit <- glm(num~.-age-restecg-thalach-fbs-exang,data=heart.data.8,family=binomial)
summary(logreg.fit)



#use train and test data to calculate the accuracy
heart.data.1<-heart.data.8
set.seed(1)
train=sample(1:nrow(heart.data.1), nrow(heart.data.1)/2) 
test=(-train)
heart.data.1.train=heart.data.1[train,]
heart.data.1.test=heart.data.1[test,]
heart.data.1.test.output=heart.data.1$num[test]
logreg.fit <- glm(num~.-age-restecg-thalach-fbs-exang,data=heart.data.1.train,family=binomial)
summary(logreg.fit)
logreg.fit.prob=predict(logreg.fit,heart.data.1.test,type="response")
head(logreg.fit.prob)
nrow(heart.data.1)
logreg.fit.pred=rep(0,nrow(heart.data.1))
logreg.fit.pred[logreg.fit.prob>.5]=1
mean(logreg.fit.pred==heart.data.1.test.output)






#use decision trees to predict num
heart.data.2<-heart.data
library(ISLR)
library(tree)
tree.heart.data.2= tree(num~.,heart.data.2)
summary(tree.heart.data.2)
plot(tree.heart.data.2)
text(tree.heart.data.2,pretty=0)

#use test and train data
set.seed(3)
train = sample(1:nrow(heart.data.2), nrow(heart.data.2)/2)
heart.data.2.test =heart.data.2[-train,]
heart.data.2.num.test = heart.data.2$num[-train]
tree.heart.data.2= tree(num~ .,heart.data.2, subset=train)
tree.pred = predict(tree.heart.data.2,heart.data.2.test,type="class")
table(tree.pred,heart.data.2.num.test)
mean(tree.pred==heart.data.2.num.test)

# use cross-validation and pruning to obtain smaller trees and their prediction accuracy 
set.seed(4)
cv.heart.data.2= cv.tree(tree.heart.data.2,FUN=prune.misclass)
names(cv.heart.data.2)
cv.heart.data.2

# use a function called prune.misclass() to obtain the best tree, which we know has a size of 8
prune.heart.data.2= prune.misclass(tree.heart.data.2,best=8)
plot(prune.heart.data.2)
text(prune.heart.data.2, pretty = 0)

tree.pred = predict(prune.heart.data.2,heart.data.2.test,type="class")
table(tree.pred,heart.data.2.num.test)
mean(tree.pred==heart.data.2.num.test)


#Linear regression of sex, thalach, exang, oldpeak, slope, ca, thal to cp
#ues regsubsets
heart.data.4=heart.data.final[,c(3,8,9,10,11,12,13,14)]


library(leaps)
regfit.full=regsubsets(cp~.,data=heart.data.4)
summary(regfit.full)

reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
which.max(reg.summary$adjr2) # function which.max() returns the index of maximum value
coef(regfit.full,5)
m1=lm(cp~ thalach+exang+ca+thal+num,data=heart.data.4) 
summary(m1)


#use KNN
set.seed(6)
train=sample(1:nrow(heart.data.4), nrow(heart.data.4)/2) # split data into two subsets
test=(-train)
# next, we create the part of x and y that will be our training data
# we will call these x.train and y.train
heart.data.4.train=heart.data.4[train,c(2,3,4,5,6,7,8)]#for all the columns
heart.data.4.test=heart.data.4[test,c(2,3,4,5,6,7,8)]

num.train=heart.data.4[train,c(1)]
num.test=heart.data.4[test,c(1)]
library(class)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=1)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=2)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=3)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=4)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=5)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=6)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=7)
table(knn.pred,num.test)
mean(knn.pred==num.test)

#relative better
set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=8)
table(knn.pred,num.test)
mean(knn.pred==num.test)

set.seed(7)
knn.pred = knn(heart.data.4.train,heart.data.4.test,num.train,k=9)
table(knn.pred,num.test)
mean(knn.pred==num.test)


#Hierarchical clustering

set.seed(5)
heart.data.3<-heart.data.final
heart.data.3 <- scale(heart.data.3)
heart.data.3<-heart.data.3*10

hc.complete=hclust(dist(heart.data.3),method="complete")
plot(hc.complete)
cutree(hc.complete,2)
hc.out.2=cutree(hc.complete, 2) 
par(mfrow=c(2,2))

#table(heart.data$age)
#table(heart.data$sex)

plot(heart.data.3[,1],heart.data.3[,2],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,1],heart.data.3[,5],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,1],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)#,##
plot(heart.data.3[,4],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)##

plot(heart.data.3[,1],heart.data.3[,3],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,4],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,6],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,1],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,3],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,4],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,5],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,2],heart.data.3[,6],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,2],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,3],heart.data.3[,4],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,5],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,6],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,3],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,4],heart.data.3[,5],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,6],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,4],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,4],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,5],heart.data.3[,6],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,5],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,5],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,6],heart.data.3[,7],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,6],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,6],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,6],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,6],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,6],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,6],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,7],heart.data.3[,8],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,7],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,7],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,7],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,7],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,7],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,8],heart.data.3[,9],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,8],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)##
plot(heart.data.3[,8],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,8],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)#
plot(heart.data.3[,8],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,9],heart.data.3[,10],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,9],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,9],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,9],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,10],heart.data.3[,11],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,10],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,10],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,11],heart.data.3[,12],col=hc.out.2,pch=hc.out.2)
plot(heart.data.3[,11],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)

plot(heart.data.3[,12],heart.data.3[,13],col=hc.out.2,pch=hc.out.2)



#Hierarchical clustering:age,sex and trestbps
set.seed(5)
heart.data.3<-heart.data.final
hc.complete=hclust(dist(heart.data.3[,c(2,3,4)]),method="complete")
plot(hc.complete)
cutree(hc.complete,2)
hc.out.2=cutree(hc.complete,2) 
par(mfrow=c(1,1))
plot(heart.data.3$trestbps,heart.data.3$sex,col=hc.out.2,pch=hc.out.2)
plot(heart.data.3$age,heart.data.3$sex,col=hc.out.2,pch=hc.out.2)
plot(heart.data.3$trestbps,heart.data.3$age,col=hc.out.2,pch=hc.out.2)















































