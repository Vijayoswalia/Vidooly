-------#importing the data-----
test = read.csv("ad_org_test.csv", stringsAsFactors = F)
train = read.csv("ad_org_train.csv", as.is = T, stringsAsFactors = F)

#View(train)
str(train)
names(train)
as.data.frame(colSums(is.na(train)))
colnames(train)

----#####changing from cahr to int------
train$adview = as.integer(train$adview)
train$views = as.integer(train$views)
train$likes = as.integer(train$likes)
train$dislikes = as.integer(train$dislikes)
train$comment = as.integer(train$comment)


-------#removing the NA values-----
which(is.na(train$views))
mean(train$dislikes, na.rm = T)
train$likes[is.na(train$likes)] <- round(mean(train$likes, na.rm = TRUE))
train$dislikes[is.na(train$dislikes)] <- round(mean(train$dislikes, na.rm = TRUE))
train$comment[is.na(train$comment)] <- round(mean(train$comment, na.rm = TRUE))
train$views[is.na(train$views)] <- round(mean(train$views, na.rm = TRUE))


----------------#####changing date to integer format-------

for (i in 1:nrow(train))
{
  train$hour[i]=0;
  train$min[i]=0;
  train$sec[i]=0;
  
  if((grepl('H',train$duration[i])==TRUE) & (grepl('M',train$duration[i])==TRUE) & (grepl('S',train$duration[i])==TRUE))
  {
    train$hour[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('H',train$duration[i])-1);
    train$min[i]=substr(train$duration[i],regexpr('H',train$duration[i])+1,regexpr('M',train$duration[i])-1);
    train$sec[i]=substr(train$duration[i],regexpr('M',train$duration[i])+1,regexpr('S',train$duration[i])-1);
  }
  
  else if((grepl('H',train$duration[i])==FALSE) & (grepl('M',train$duration[i])==TRUE) & (grepl('S',train$duration[i])==TRUE))
  {
    train$min[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('M',train$duration[i])-1);
    train$sec[i]=substr(train$duration[i],regexpr('M',train$duration[i])+1,regexpr('S',train$duration[i])-1);
  }
  else if((grepl('H',train$duration[i])==FALSE) & (grepl('M',train$duration[i])==FALSE) & (grepl('S',train$duration[i])==TRUE))
  {
    train$sec[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('S',train$duration[i])-1);
  }
  else if((grepl('H',train$duration[i])==TRUE) & (grepl('M',train$duration[i])==TRUE) & (grepl('S',train$duration[i])==FALSE))
  {
    train$hour[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('H',train$duration[i])-1);
    train$min[i]=substr(train$duration[i],regexpr('H',train$duration[i])+1,regexpr('M',train$duration[i])-1);
    
  }
  else if((grepl('H',train$duration[i])==TRUE) & (grepl('M',train$duration[i])==FALSE) & (grepl('S',train$duration[i])==FALSE))
  {
    train$hour[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('H',train$duration[i])-1);
    
  }
  else if((grepl('H',train$duration[i])==FALSE) & (grepl('M',train$duration[i])==TRUE) & (grepl('S',train$duration[i])==FALSE))
  {
    train$min[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('M',train$duration[i])-1);
    
  }
  else if((grepl('H',train$duration[i])==TRUE) & (grepl('M',train$duration[i])==FALSE) & (grepl('S',train$duration[i])==TRUE))
  {
    train$hour[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('H',train$duration[i])-1);
    train$sec[i]=substr(train$duration[i],regexpr('H',train$duration[i])+1,regexpr('S',train$duration[i])-1);
  }
}

train$hour = as.integer(train$hour)
train$min = as.integer(train$min)
train$sec = as.integer(train$sec)
train$new_duration=(train$hour*3600)+(train$min*60)+(train$sec)

-----------#treating outliers#---------------
names(train)
boxplot(train$views)
boxplot(train$likes)
boxplot(train$dislikes)
boxplot(train$comment)
boxplot(train$new_duration)

boxplot.stats(train$views)$out
boxplot.stats(train$likes)$out
boxplot.stats(train$dislikes)$out
boxplot.stats(train$comment)$out

for (j in c(3,4,5,6,13)){
  r1=quantile(train[,j],0.25)-(1.5*IQR(train[,j]))
  r1
  r2=quantile(train[,j],0.75)+(1.5*IQR(train[,j]))
  r2
  for (i in 1:nrow(train)){
    if ((train[i,j]<r1) | (train[i,j]>r2)){
      train[i,j]=median(train[,j])
    } 
  }
}

library(car)
library(caTools)
spl=sample.split(train$views,0.80)
train_set=subset(train,spl==TRUE)
test_set=subset(train,spl==FALSE)


---------#modelling using cart
  
  
library(car)
library(rpart)
library(rpart.plot)
modelcart = rpart(adview ~ views+likes+dislikes+comment+new_duration,data=train_set, method="anova")
modelcart
rpart.plot(modelcart, type=3, extra=101, fallen.leaves=T)
predcart = predict(modelcart, test_set)
predcart
C = table(predictions = predcart, actual=test_set$adview)
C
sum(diag(C)/sum(C))
printcp(modelcart)
plotcp(modelcart)
rsq.rpart(modelcart)

---------#modelling Random Forest

library(randomForest)
modelrandom = randomForest(adview ~ views+likes+dislikes+comment+new_duration,data=train_set, mtry=5, ntree=2000)
modelrandom
summary(modelrandom)	
importance(modelrandom)
varImpPlot(modelrandom)
predrandom = predict(modelrandom, test_set)
predrandom
t = table(predictions = predrandom, actual=test_set$adview)
t
test_set$adview
sum(diag(t)/sum(t))


--------------     #for test data -------------------
  str(train)
as.data.frame(colSums(is.na(test)))
colnames(test)


test$views = as.integer(test$views)
test$likes = as.integer(test$likes)
test$dislikes = as.integer(test$dislikes)
test$comment = as.integer(test$comment)

test$likes[is.na(test$likes)] <- round(mean(test$likes, na.rm = TRUE))
test$dislikes[is.na(test$dislikes)] <- round(mean(test$dislikes, na.rm = TRUE))
test$comment[is.na(test$comment)] <- round(mean(test$comment, na.rm = TRUE))
test$views[is.na(test$views)] <- round(mean(test$views, na.rm = TRUE))


for (i in 1:nrow(test))
{
  test$hour[i]=0;
  test$min[i]=0;
  test$sec[i]=0;
  
  if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$min[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('M',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
  
  else if((grepl('H',test$duration[i])==FALSE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$min[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('M',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
  else if((grepl('H',train$duration[i])==FALSE) & (grepl('M',train$duration[i])==FALSE) & (grepl('S',train$duration[i])==TRUE))
  {
    train$sec[i]=substr(train$duration[i],regexpr('T',train$duration[i])+1,regexpr('S',train$duration[i])-1);
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$min[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==FALSE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==FALSE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$min[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==FALSE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
}

test$hour = as.integer(test$hour)
test$min = as.integer(test$min)
test$sec = as.integer(test$sec)
test$new_duration=(test$hour*3600)+(test$min*60)+(test$sec)

for (j in c(2,3,4,5,12)){
  r1=quantile(test[,j],0.25)-(1.5*IQR(test[,j]))
  r1
  r2=quantile(test[,j],0.75)+(1.5*IQR(test[,j]))
  r2
  for (i in 1:nrow(test)){
    if ((test[i,j]<r1) | (test[i,j]>r2)){
      test[i,j]=median(test[,j])
    } 
  }
}


-------######final model and saving output in csv-----------

predrandom = predict(modelrandom, test)


redcart = predict(modelcart, test)
sub1 = read.csv("ad_org_test.csv")
sub1$adview = round(predcart) #prediction using cart
sub1$adviews = round(predrandom) #predictions using RandomForest
write.csv(sub1,"Solution_ad_org.csv",row.names = F)
