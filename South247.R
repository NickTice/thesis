# Loading the necessary packages
library('rvest')
library('stringr')
library('xlsx')
library(OptimalCutpoints)

z=read.xlsx(file.choose(),1)
south247=subset(z, select = -c(Column1, Year, Player, HighSchool))
names(south247) = c("Grade", "Position", "State", "Height", "Weight", "Drafted", "Private", "Enrollment", "AllBoys", "Minority", "EconomicDis", "Graduation")

south247$Northeast=0
for (x in which(south247$State %in% c("ME","NH","MA","RI","CT","VT","NY","PA","NJ", "DC","DE", "MD"))) {
  south247[x,13]=1
}

south247$South=0
for (x in which(south247$State %in% c("WV","VA","KY","TN","NC","SC","GA","AL","MS","AR","FL","LA"))){
  south247[x,14]=1
}

south247$Southwest=0
for (x in which(south247$State %in% c("TX","OK", "NM", "AZ","UT","NV"))){
  south247[x,15]=1
}

south247$Midwest=0
for (x in which(south247$State %in% c("OH","IN","MI","IL","MO","WI","MN","IA","KS","NE","SD","ND"))){
  south247[x,16]=1
}

south247$West=0
for (x in which(south247$State %in% c("CO","WY","MT","ID","WA","OR","CA","AK","HI"))){
  south247[x,17]=1
}


# Dummy coding for positions
south247$ATH=0
for (x in which(south247$Position %in% "ATH")){
  south247[x,18]=1
}

south247$QB=0
for (x in which(south247$Position %in% c("DUAL", "PRO"))){
  south247[x,19]=1
}

south247$OL=0
for (x in which(south247$Position %in% c("OC","OG","OT"))){
  south247[x,20]=1
}

south247$RB=0
for (x in which(south247$Position %in% c("RB","APB"))){
  south247[x,21]=1
}

south247$REC=0
for (x in which(south247$Position %in% c("WR","TE"))){
  south247[x,22]=1
}

south247$DL=0
for (x in which(south247$Position %in% c("SDE","WDE","DT"))){
  south247[x,23]=1
}

south247$LB=0
for (x in which(south247$Position %in% c("ILB","OLB"))){
  south247[x,24]=1
}

south247$DB=0
for (x in which(south247$Position %in% c("S","CB"))){
  south247[x,25]=1
}

south247=subset(south247, subset = (South==1))
south247=subset(south247, subset = (Private==0))
south247=na.omit(south247)

south247=subset(south247, select = -c(Position, State, Private, AllBoys, Northeast, South, Southwest, Midwest, West))
south247=south247[c(1:3,5:16,4)]
south247=subset(south247, select = -c(ATH))


library(bestglm)
south247AIC=bestglm(south247, IC="AIC", family = binomial, TopModels = 10 )
#south247BIC=bestglm(south247, IC="BIC", family = binomial, TopModels = 5 )
south247AIC$BestModels
south247BIC$BestModels

fit.null=glm(Drafted~ Grade + Height + Weight, data = south247, family = "binomial")
fit1=glm(Drafted~ Grade + Height + Weight + REC + LB, data = south247, family = "binomial")
fit2=glm(Drafted~ Grade + Height + Weight + REC, data = south247, family = "binomial")
fit3=glm(Drafted~ Grade + Height + Weight + RB + REC + LB, data = south247, family = "binomial")
fit4=glm(Drafted~ Grade + Height + Weight + RB + REC, data = south247, family = "binomial")
fit5=glm(Drafted~ Grade + Height + Weight + Graduation + REC + LB, data = south247, family = "binomial")

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

compareGLM(fit.null,fit1,fit2,fit3,fit4,fit5,fit.test)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)


# Hosmer-Lemoshow Test
# p-value over 0.05 indicates a good fit.
library(ResourceSelection)
hoslem.test(south247$Drafted, fitted(fit1), g=10)
hoslem.test(south247$Drafted, fitted(fit2), g=10)
hoslem.test(south247$Drafted, fitted(fit3), g=10)
hoslem.test(south247$Drafted, fitted(fit4), g=10)
hoslem.test(south247$Drafted, fitted(fit5), g=10)
# Reject 4

# Likelihood ratio test
lrtest(fit.null,fit2)
lrtest(fit1,fit5)
lrtest(fit2,fit5)

#Wald Test
regTermTest(fit1, "LB")

#Fit 2 smallest BIC, similar AIC, all predictors are significant based on walds test and model is best based on LR test.




# Creates predicted variables
predicted1 <- predict(fit1, south247, type="response")
predicted2 <- predict(fit2, south247, type="response")
predicted3 <- predict(fit3, south247, type="response")
predicted4 <- predict(fit4, south247, type="response")
predicted5 <- predict(fit5, south247, type="response")

library(InformationValue)
# provides ways to find the optimal cutoff to improve the prediction of 1's, 0's, 
# both 1's and 0's and o reduce the misclassification error.
optCutOff1 <- optimalCutoff(south247$Drafted, predicted1)
optCutOff2 <- optimalCutoff(south247$Drafted, predicted2)
optCutOff3 <- optimalCutoff(south247$Drafted, predicted3)
optCutOff4 <- optimalCutoff(south247$Drafted, predicted4)
optCutOff5 <- optimalCutoff(south247$Drafted, predicted5)

# Misclassification error is the percentage mismatch of predcited vs actuals, irrespective 
# of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(south247$Drafted, predicted1, threshold = optCutOff1)
misClassError(south247$Drafted, predicted2, threshold = optCutOff2)
misClassError(south247$Drafted, predicted3, threshold = optCutOff3)
misClassError(south247$Drafted, predicted4, threshold = optCutOff4)
misClassError(south247$Drafted, predicted5, threshold = optCutOff5)

# traces the percentage of true positives accurately predicted by a given logit model as the 
# prediction probability cutoff is lowered from 1 to 0. Greater area under the better.
plotROC(south247$Drafted, predicted1)
plotROC(south247$Drafted, predicted2)
plotROC(south247$Drafted, predicted3)
plotROC(south247$Drafted, predicted4)
plotROC(south247$Drafted, predicted5)


sensitivity(south247$Drafted, predicted1, threshold = optCutOff1)
specificity(south247$Drafted, predicted1, threshold = optCutOff1)
precision(south247$Drafted, predicted1, threshold = optCutOff1)
confusionMatrix(south247$Drafted, predicted1, optCutOff1)

sensitivity(south247$Drafted, predicted2, threshold = optCutOff2)
specificity(south247$Drafted, predicted2, threshold = optCutOff2)
precision(south247$Drafted, predicted2, threshold = optCutOff2)
confusionMatrix(south247$Drafted, predicted2, optCutOff2)


sensitivity(south247$Drafted, predicted3, threshold = optCutOff3)
specificity(south247$Drafted, predicted3, threshold = optCutOff3)
precision(south247$Drafted, predicted3, threshold = optCutOff3)
confusionMatrix(south247$Drafted, predicted3, optCutOff3)


sensitivity(south247$Drafted, predicted4, threshold = optCutOff4)
specificity(south247$Drafted, predicted4, threshold = optCutOff4)
precision(south247$Drafted, predicted4, threshold = optCutOff4)
confusionMatrix(south247$Drafted, predicted4, optCutOff4)

sensitivity(south247$Drafted, predicted5, threshold = optCutOff5)
specificity(south247$Drafted, predicted5, threshold = optCutOff5)
precision(south247$Drafted, predicted5, threshold = optCutOff5)
confusionMatrix(south247$Drafted, predicted5, optCutOff5)

# Final model

south247.log=glm(Drafted~ Grade + Height + Weight + REC, data = south247, family = "binomial")
predicted.south247 <- predict(south247.log, south247, type="response")

optCutOff.south247 <- optimalCutoff(south247$Drafted, predicted.south247)


misClassError(south247$Drafted, predicted.south247, threshold = optCutOff.south247)

plotROC(south247$Drafted, predicted.south247)


sensitivity(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
specificity(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
precision(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
confusionMatrix(south247$Drafted, predicted.south247, optCutOff.south247)

# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/#computing-penalized-logistic-regression
x=model.matrix(Drafted~.,south247)[,-1]
y=south247$Drafted
# alpha 1 for lasso 0 for ridge.
# In penalized regression, you need to specify a constant lambda to adjust the amount of the coefficient shrinkage. The best lambda for your data, can be defined as the lambda that minimize the cross-validation prediction error rate. 
# This can be determined automatically using the function cv.glmnet().

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model=glmnet(x, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
# Make prediction on test data
probabilities <- model %>% predict(newx = x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- south247$Drafted
mean(predicted.classes == observed.classes)
coef(model)

full.model <- glm(Drafted ~., data = south247, family = binomial)
# Make predictions
probabilities <- full.model %>% predict(south247, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
# Model accuracy
observed.classes <- south247$Drafted
mean(predicted.classes == observed.classes)



library(tree)
#SVM


ind = sample(2, nrow(south247), replace=TRUE, prob=c(0.8,.2))
trainData = south247
#testData = south247[ind==2,]

DraftStatus = ifelse(trainData$Drafted==1, "Drafted", "Undrafted")
trainData=data.frame(trainData, DraftStatus)
#DraftStatus = ifelse(testData$Drafted==1, "Drafted", "Undrafted")
#testData=data.frame(testData, DraftStatus)
trainData=subset(trainData, select = -Drafted)
#testData=subset(testData, select = -Drafted)
tree.train=tree(DraftStatus ~ ., data = trainData)
summary(tree.train)
plot(tree.train)
text(tree.train, pretty = 0)

#DraftStatus = ifelse(testData$Drafted==1, "Drafted", "Undrafted")
#testData=data.frame(testData, DraftStatus2)

train.pred=predict(tree.train, trainData, type = "class")
#test.pred=predict(tree.train, testData, type="class")

cm1=table(predicted=train.pred, actual=trainData$DraftStatus)
(sum(diag(cm1)))/sum(cm1)
cm1[2,2]/(cm1[2,1]+cm1[2,2])

#cm2=table(predicted=test.pred, actual=testData$DraftStatus)
#(sum(diag(cm2)))/sum(cm2)
#cm2[2,2]/(cm2[2,1]+cm2[2,2])

train.cv = cv.tree(tree.train, FUN = prune.misclass)
min_idx=which.min(train.cv$dev)
train.cv$size[min_idx]

par(mfrow = c(1, 1))

plot(train.cv)
# better plot
plot(train.cv$size, train.cv$dev / nrow(trainData), type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate")

train.prune= prune.misclass(tree.train, best =3 )
summary(train.prune)

plot(train.prune)
text(train.prune, pretty = 0)
title(main = "Pruned Classification Tree")

train.prune.pred = predict(train.prune, trainData, type = "class")
cm3=table(predicted = train.prune.pred, actual = trainData$DraftStatus)

(sum(diag(cm3)))/sum(cm3)
cm3[2,2]/(cm3[2,1]+cm3[2,2])

test.prune.pred = predict(train.prune, testData, type= "class")
cm4=table(predicted = test.prune.pred, actual = testData$DraftStatus)
(sum(diag(cm4)))/sum(cm4)
cm4[2,2]/(cm4[2,1]+cm4[2,2])

trainData2=trainData
finalsouth247tree=train.prune
plot(finalsouth247tree)
text(finalsouth247tree, pretty = 0)
title(main = "South247 Classification Tree")

south247.pred = predict(finalsouth247tree, trainData2, type = "class")
south247.cm=table(predicted = south247.pred, actual = trainData2$DraftStatus)

(sum(diag(cm3)))/sum(cm3)
cm3[2,2]/(cm3[2,1]+cm3[2,2])



library(randomForest)

rf.south = randomForest(DraftStatus~., data=trainData, proximity=T)
rf.south
table(predict(rf.south), trainData$DraftStatus)

importance(rf.south)
varImpPlot(rf.south)
