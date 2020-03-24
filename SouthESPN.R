library('rvest')
library('stringr')
library('xlsx')
library(bestglm)


y=file.choose()
finalESPN1=read.xlsx(y,1)
southESPN=finalESPN1
southESPN = southESPN[,-1]
southESPN = southESPN[,-2]
names(southESPN) = c("Grade", "Position", "State", "Height", "Weight", "Drafted", "Private", "Enrollment", "AllBoys", "Minority", "EconomicDis", "Graduation")
head(subset(southESPN, select = 'Position'))
southESPN$State=trimws(southESPN$State)


southESPN$Northeast=0
for (x in which(southESPN$State %in% c("ME","NH","MA","RI","CT","VT","NY","PA","NJ", "DC","DE", "MD"))) {
  southESPN[x,13]=1
}

southESPN$South=0
for (x in which(southESPN$State %in% c("WV","VA","KY","TN","NC","SC","GA","AL","MS","AR","FL","LA"))){
  southESPN[x,14]=1
}

southESPN$Southwest=0
for (x in which(southESPN$State %in% c("TX","OK", "NM", "AZ","UT","NV"))){
  southESPN[x,15]=1
}


southESPN$Midwest=0
for (x in which(southESPN$State %in% c("OH","IN","MI","IL","MO","WI","MN","IA","KS","NE","SD","ND"))){
  southESPN[x,16]=1
}


southESPN$West=0
for (x in which(southESPN$State %in% c("CO","WY","MT","ID","WA","OR","CA","AK","HI"))){
  southESPN[x,17]=1
}

southESPN$ATH=0
for (x in which(southESPN$Position %in% "ATH")){
  southESPN[x,18]=1
}

southESPN$QB=0
for (x in which(southESPN$Position %in% c("QB", "QB-DT", "QB-PP"))){
  southESPN[x,19]=1
}

southESPN$OL=0
for (x in which(southESPN$Position %in% c("OC","OG","OT"))){
  southESPN[x,20]=1
}

southESPN$RB=0
for (x in which(southESPN$Position %in% c("RB"))){
  southESPN[x,21]=1
}

southESPN$REC=0
for (x in which(southESPN$Position %in% c("WR","TE", "TE-H", "TE-Y"))){
  southESPN[x,22]=1
}

southESPN$DL=0
for (x in which(southESPN$Position %in% c("DE","DT"))){
  southESPN[x,23]=1
}

southESPN$LB=0
for (x in which(southESPN$Position %in% c("ILB","OLB"))){
  southESPN[x,24]=1
}

southESPN$DB=0
for (x in which(southESPN$Position %in% c("S","CB"))){
  southESPN[x,25]=1
}


southESPN <- southESPN[!duplicated(southESPN),]

southESPN$EconomicDis=as.numeric(southESPN$EconomicDis)
southESPN=subset(southESPN, select = -c(State, Position))
southESPN=southESPN[c(1:3,5:23,4)]
southESPN=subset(southESPN, subset = (Private==0))
southESPN=na.omit(southESPN)
southESPN=subset(southESPN, subset = (South==1))
southESPN=subset(southESPN, select = -c(Northeast, South, Southwest, Midwest, West, Private,AllBoys,ATH))

modelsSouthESPN=bestglm(southESPN, IC="AIC", family = binomial,TopModels = 10)
modelsSouthESPN$BestModels

fit1=glm(Drafted~ Grade + Height + Minority + OL + RB, data = southESPN, family = "binomial")
fit2=glm(Drafted~ Grade + Height + OL + RB, data = southESPN, family = "binomial")
fit3=glm(Drafted~ Grade + Height + EconomicDis + OL + RB, data = southESPN, family = "binomial")
fit4=glm(Drafted~ Grade + Height + Minority + OL + RB + LB, data = southESPN, family = "binomial")
fit5=glm(Drafted~ Grade + Height + Graduation + OL + RB + LB, data = southESPN, family = "binomial")

compareGLM(fit1,fit2,fit3,fit4,fit5)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)



library(glmnet)
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/#computing-penalized-logistic-regression
x=model.matrix(Drafted~.,southESPN)[,-1]
y=southESPN$Drafted
# alpha 1 for lasso 0 for ridge.
# In penalized regression, you need to specify a constant lambda to adjust the amount of the coefficient shrinkage. The best lambda for your data, can be defined as the lambda that minimize the cross-validation prediction error rate. 
# This can be determined automatically using the function cv.glmnet().

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model=glmnet(x, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
# Make prediction on test data
probabilities <- model %>% predict(newx = x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- southESPN$Drafted
coef(model)
mean(predicted.classes == observed.classes)

full.model <- glm(Drafted ~., data = southESPN, family = binomial)
# Make predictions
probabilities <- full.model %>% predict(southESPN, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
# Model accuracy
observed.classes <- southESPN$Drafted
mean(predicted.classes == observed.classes)


# fail top reject second model is better
anova(fit4,fit5, test="Chisq")

# Hosmer-Lemoshow Test
# p-value over 0.05 indicates a good fit.
library(ResourceSelection)
hoslem.test(southESPN$Drafted, fitted(fit1), g=6)
hoslem.test(southESPN$Drafted, fitted(fit2), g=5)
hoslem.test(southESPN$Drafted, fitted(fit3), g=6)
hoslem.test(southESPN$Drafted, fitted(fit4), g=7)
hoslem.test(southESPN$Drafted, fitted(fit5), g=7)
# Reject 3 and 4

anova(fit1, "Chisq")
anova(fit2, "Chisq")
anova(fit3, "Chisq")
anova(fit4, "Chisq")
anova(fit5, "Chisq")

# Likelihood ratio test
lrtest(fit1,fit2)
lrtest(fit1,fit4)
lrtest(fit2,fit5)

#Wald Test
regTermTest(fit2, "RB")

#Fit 2 has second lowest AIC, lowest BIC. Predictors are inlcuded in every model. Every predictor is significant. LR test it is better than fit1
# fit 2 has better specificity, sensitivity and precision



predicted1 <- predict(fit1, southESPN, type="response")
predicted2 <- predict(fit2, southESPN, type="response")
predicted3 <- predict(fit3, southESPN, type="response")
predicted4 <- predict(fit4, southESPN, type="response")
predicted5 <- predict(fit5, southESPN, type="response")

library(InformationValue)
# provides ways to find the optimal cutoff to improve the prediction of 1's, 0's, 
# both 1's and 0's and o reduce the misclassification error.
optCutOff1 <- optimalCutoff(southESPN$Drafted, predicted1)
optCutOff2 <- optimalCutoff(southESPN$Drafted, predicted2)
optCutOff3 <- optimalCutoff(southESPN$Drafted, predicted3)
optCutOff4 <- optimalCutoff(southESPN$Drafted, predicted4)
optCutOff5 <- optimalCutoff(southESPN$Drafted, predicted5)

# Misclassification error is the percentage mismatch of predcited vs actuals, irrespective 
# of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(southESPN$Drafted, predicted1, threshold = optCutOff1)
misClassError(southESPN$Drafted, predicted2, threshold = optCutOff2)
misClassError(southESPN$Drafted, predicted3, threshold = optCutOff3)
misClassError(southESPN$Drafted, predicted4, threshold = optCutOff4)
misClassError(southESPN$Drafted, predicted5, threshold = optCutOff5)

# traces the percentage of true positives accurately predicted by a given logit model as the 
# prediction probability cutoff is lowered from 1 to 0. Greater area under the better.
plotROC(southESPN$Drafted, predicted1)
plotROC(southESPN$Drafted, predicted2)
plotROC(southESPN$Drafted, predicted3)
plotROC(southESPN$Drafted, predicted4)
plotROC(southESPN$Drafted, predicted5)


sensitivity(southESPN$Drafted, predicted1, threshold = optCutOff1)
specificity(southESPN$Drafted, predicted1, threshold = optCutOff1)
precision(southESPN$Drafted, predicted1, threshold = optCutOff1)
confusionMatrix(southESPN$Drafted, predicted1, optCutOff1)

sensitivity(southESPN$Drafted, predicted2, threshold = optCutOff2)
specificity(southESPN$Drafted, predicted2, threshold = optCutOff2)
precision(southESPN$Drafted, predicted2, threshold = optCutOff2)
confusionMatrix(southESPN$Drafted, predicted2, optCutOff2)


sensitivity(southESPN$Drafted, predicted3, threshold = optCutOff3)
specificity(southESPN$Drafted, predicted3, threshold = optCutOff3)
precision(southESPN$Drafted, predicted3, threshold = optCutOff3)
confusionMatrix(southESPN$Drafted, predicted3, optCutOff3)


sensitivity(southESPN$Drafted, predicted4, threshold = optCutOff4)
specificity(southESPN$Drafted, predicted4, threshold = optCutOff4)
precision(southESPN$Drafted, predicted4, threshold = optCutOff4)
confusionMatrix(southESPN$Drafted, predicted4, optCutOff4)

sensitivity(southESPN$Drafted, predicted5, threshold = optCutOff5)
specificity(southESPN$Drafted, predicted5, threshold = optCutOff5)
precision(southESPN$Drafted, predicted5, threshold = optCutOff5)
confusionMatrix(southESPN$Drafted, predicted5, optCutOff5)

# Final model

southESPN.log=glm(Drafted~ Grade + Height + OL + RB, data = southESPN, family = "binomial")
predicted.southESPN <- predict(southESPN.log, southESPN, type="response")

optCutOff.southESPN <- optimalCutoff(southESPN$Drafted, predicted.southESPN)


misClassError(southESPN$Drafted, predicted.southESPN, threshold = optCutOff.southESPN)

plotROC(southESPN$Drafted, predicted.southESPN)


sensitivity(southESPN$Drafted, predicted.southESPN, threshold = optCutOff.southESPN)
specificity(southESPN$Drafted, predicted.southESPN, threshold = optCutOff.southESPN)
precision(southESPN$Drafted, predicted.southESPN, threshold = optCutOff.southESPN)
confusionMatrix(southESPN$Drafted, predicted.southESPN, optCutOff.southESPN)




## Decision Tree

library(tree)

southESPN$Drafted=as.factor(southESPN$Drafted)

ind = sample(2, nrow(southESPN), replace=TRUE, prob=c(0.7,0.3))
trainData = southESPN
#testESPN.S = southESPN[ind==2,]
DraftStatus = ifelse(trainData$Drafted==1, "Drafted", "Undrafted")
trainData=data.frame(trainData, DraftStatus)
#DraftStatus = ifelse(testData$Drafted==1, "Drafted", "Undrafted")
#testData=data.frame(testData, DraftStatus)
trainData=subset(trainData, select = -Drafted)
#testData=subset(testData, select = -Drafted)


tree.train=tree(DraftStatus ~., data = trainData)
summary(tree.train)
plot(tree.train)
text(tree.train, pretty = 0)


train.pred=predict(tree.train, trainData, type = "class")
#test.pred=predict(tree.train, testData, type="class")

cm1=table(predicted=train.pred, actual=trainData$DraftStatus)
(sum(diag(cm1)))/sum(cm1)
cm1[2,2]/(cm1[2,1]+cm1[2,2])


cm2=table(predicted=test.pred, actual=testData$DraftStatus)
(sum(diag(cm2)))/sum(cm2)
cm2[2,2]/(cm2[2,1]+cm2[2,2])

train.cv = cv.tree(tree.train, FUN = prune.misclass)
min_idx=which.min(train.cv$dev)
train.cv$size[min_idx]

par(mfrow = c(1, 1))

plot(train.cv)
# better plot
plot(train.cv$size, train.cv$dev / nrow(trainData), type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate")

train.prune= prune.misclass(tree.train, best = 4)
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

finalsouthESPNtree=train.prune
finalsouthESPNtree
trainData4=trainData

plot(finalsouthESPNtree)
text(finalsouthESPNtree, pretty = 0)
title(main = "southESPN Classification Tree")
southESPN.pred = predict(finalsouthESPNtree,trainData4, type= "class")
cm4=table(predicted = southESPN.pred, actual = trainData4$DraftStatus)
cm4
(sum(diag(cm4)))/sum(cm4)
cm4[2,2]/(cm4[2,1]+cm4[2,2])
