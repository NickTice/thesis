library('rvest')
library('stringr')
library('xlsx')
library(bestglm)


y=file.choose()
finalESPN1=read.xlsx(y,1)
publicESPN=finalESPN1
publicESPN = publicESPN[,-1]
publicESPN = publicESPN[,-2]
names(publicESPN) = c("Grade", "Position", "State", "Height", "Weight", "Drafted", "Private", "Enrollment", "AllBoys", "Minority", "EconomicDis", "Graduation")
head(subset(publicESPN, select = 'Position'))
publicESPN$State=trimws(publicESPN$State)


publicESPN$Northeast=0
for (x in which(publicESPN$State %in% c("ME","NH","MA","RI","CT","VT","NY","PA","NJ", "DC","DE", "MD"))) {
  publicESPN[x,13]=1
}

publicESPN$South=0
for (x in which(publicESPN$State %in% c("WV","VA","KY","TN","NC","SC","GA","AL","MS","AR","FL","LA"))){
  publicESPN[x,14]=1
}

publicESPN$Southwest=0
for (x in which(publicESPN$State %in% c("TX","OK", "NM", "AZ","UT","NV"))){
  publicESPN[x,15]=1
}


publicESPN$Midwest=0
for (x in which(publicESPN$State %in% c("OH","IN","MI","IL","MO","WI","MN","IA","KS","NE","SD","ND"))){
  publicESPN[x,16]=1
}


publicESPN$West=0
for (x in which(publicESPN$State %in% c("CO","WY","MT","ID","WA","OR","CA","AK","HI"))){
  publicESPN[x,17]=1
}


publicESPN$OFF=0
for (x in which(publicESPN$Position %in% c("QB", "QB-DT", "QB-PP","OC","OG","OT","RB","WR","TE", "TE-H", "TE-Y"))){
  publicESPN[x,18]=1
}


publicESPN$DEF=0
for (x in which(publicESPN$Position %in% c("DE","DT","ILB","OLB","S","CB"))){
  publicESPN[x,19]=1
}


publicESPN <- publicESPN[!duplicated(publicESPN),]

publicESPN$EconomicDis=as.numeric(publicESPN$EconomicDis)

publicESPN=subset(publicESPN, select = -c(State, Position))
publicESPN=publicESPN[c(1:3,5:17,4)]
publicESPN=subset(publicESPN, subset = (Private==0))
publicESPN=na.omit(publicESPN)

publicESPN=subset(publicESPN, select = -c(Private,AllBoys))
modelspublicESPN=bestglm(publicESPN, IC="AIC", family = binomial,TopModels = 10)
modelspublicESPN$BestModels


fit1=glm(Drafted~ Grade + Height + Minority + Graduation + Southwest, data = publicESPN, family = "binomial")
fit2=glm(Drafted~ Grade + Height + Minority + Graduation + Southwest + DEF, data = publicESPN, family = "binomial")
fit3=glm(Drafted~ Grade + Height + Minority + Graduation + Southwest + OFF + DEF, data = publicESPN, family = "binomial")
fit4=glm(Drafted~ Grade + Height + Graduation + Southwest, data = publicESPN, family = "binomial")
fit5=glm(Drafted~ Grade + Height + Graduation + Southwest + OFF + DEF, data = publicESPN, family = "binomial")

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
x=model.matrix(Drafted~.,publicESPN)[,-1]
y=publicESPN$Drafted
# alpha 1 for lasso 0 for ridge.
# In penalized regression, you need to specify a constant lambda to adjust the amount of the coefficient shrinkage. The best lambda for your data, can be defined as the lambda that minimize the cross-validation prediction error rate. 
# This can be determined automatically using the function cv.glmnet().

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model=glmnet(x, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
# Make prediction on test data
probabilities <- model %>% predict(newx = x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- publicESPN$Drafted
coef(model)
mean(predicted.classes == observed.classes)

full.model <- glm(Drafted ~., data = publicESPN, family = binomial)
# Make predictions
probabilities <- full.model %>% predict(publicESPN, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
# Model accuracy
observed.classes <- publicESPN$Drafted
mean(predicted.classes == observed.classes)


# fail top reject second model is better
anova(fit4,fit5, test="Chisq")

# Hosmer-Lemoshow Test
# p-value over 0.05 indicates a good fit.
library(ResourceSelection)
hoslem.test(publicESPN$Drafted, fitted(fit1), g=6)
hoslem.test(publicESPN$Drafted, fitted(fit2), g=7)
hoslem.test(publicESPN$Drafted, fitted(fit3), g=8)
hoslem.test(publicESPN$Drafted, fitted(fit4), g=5)
hoslem.test(publicESPN$Drafted, fitted(fit5), g=7)
# Reject 3 and 4

anova(fit1, fit4,"Chisq")
anova(fit2, "Chisq")
anova(fit3, "Chisq")
anova(fit4, "Chisq")
anova(fit5, "Chisq")

# Likelihood ratio test
lrtest(fit1,fit2)
lrtest(fit1,fit4)
lrtest(fit2,fit5)

#Wald Test

regTermTest(fit1, "Graduation")

# Fit 1 smallest AIC, second lowest BIC. Other model to consider is fit 4. 1 has better senseitivity, precision, and accuracy
# 




predicted1 <- predict(fit1, publicESPN, type="response")
predicted2 <- predict(fit2, publicESPN, type="response")
predicted3 <- predict(fit3, publicESPN, type="response")
predicted4 <- predict(fit4, publicESPN, type="response")
predicted5 <- predict(fit5, publicESPN, type="response")

library(InformationValue)
# provides ways to find the optimal cutoff to improve the prediction of 1's, 0's, 
# both 1's and 0's and o reduce the misclassification error.
optCutOff1 <- optimalCutoff(publicESPN$Drafted, predicted1)
optCutOff2 <- optimalCutoff(publicESPN$Drafted, predicted2)
optCutOff3 <- optimalCutoff(publicESPN$Drafted, predicted3)
optCutOff4 <- optimalCutoff(publicESPN$Drafted, predicted4)
optCutOff5 <- optimalCutoff(publicESPN$Drafted, predicted5)

# Misclassification error is the percentage mismatch of predcited vs actuals, irrespective 
# of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(publicESPN$Drafted, predicted1, threshold = optCutOff1)
misClassError(publicESPN$Drafted, predicted2, threshold = optCutOff2)
misClassError(publicESPN$Drafted, predicted3, threshold = optCutOff3)
misClassError(publicESPN$Drafted, predicted4, threshold = optCutOff4)
misClassError(publicESPN$Drafted, predicted5, threshold = optCutOff5)

# traces the percentage of true positives accurately predicted by a given logit model as the 
# prediction probability cutoff is lowered from 1 to 0. Greater area under the better.
plotROC(publicESPN$Drafted, predicted1)
plotROC(publicESPN$Drafted, predicted2)
plotROC(publicESPN$Drafted, predicted3)
plotROC(publicESPN$Drafted, predicted4)
plotROC(publicESPN$Drafted, predicted5)


sensitivity(publicESPN$Drafted, predicted1, threshold = optCutOff1)
specificity(publicESPN$Drafted, predicted1, threshold = optCutOff1)
precision(publicESPN$Drafted, predicted1, threshold = optCutOff1)
confusionMatrix(publicESPN$Drafted, predicted1, optCutOff1)

sensitivity(publicESPN$Drafted, predicted2, threshold = optCutOff2)
specificity(publicESPN$Drafted, predicted2, threshold = optCutOff2)
precision(publicESPN$Drafted, predicted2, threshold = optCutOff2)
confusionMatrix(publicESPN$Drafted, predicted2, optCutOff2)


sensitivity(publicESPN$Drafted, predicted3, threshold = optCutOff3)
specificity(publicESPN$Drafted, predicted3, threshold = optCutOff3)
precision(publicESPN$Drafted, predicted3, threshold = optCutOff3)
confusionMatrix(publicESPN$Drafted, predicted3, optCutOff3)


sensitivity(publicESPN$Drafted, predicted4, threshold = optCutOff4)
specificity(publicESPN$Drafted, predicted4, threshold = optCutOff4)
precision(publicESPN$Drafted, predicted4, threshold = optCutOff4)
confusionMatrix(publicESPN$Drafted, predicted4, optCutOff4)

sensitivity(publicESPN$Drafted, predicted5, threshold = optCutOff5)
specificity(publicESPN$Drafted, predicted5, threshold = optCutOff5)
precision(publicESPN$Drafted, predicted5, threshold = optCutOff5)
confusionMatrix(publicESPN$Drafted, predicted5, optCutOff5)

# Final model

publicESPN.log=glm(Drafted~ Grade + Height + Minority + Graduation + Southwest, data = publicESPN, family = "binomial")
predicted.publicESPN <- predict(publicESPN.log, publicESPN, type="response")

optCutOff.publicESPN <- optimalCutoff(publicESPN$Drafted, predicted.publicESPN)


misClassError(publicESPN$Drafted, predicted.publicESPN, threshold = optCutOff.publicESPN)

plotROC(publicESPN$Drafted, predicted.publicESPN)


sensitivity(publicESPN$Drafted, predicted.publicESPN, threshold = optCutOff.publicESPN)
specificity(publicESPN$Drafted, predicted.publicESPN, threshold = optCutOff.publicESPN)
precision(publicESPN$Drafted, predicted.publicESPN, threshold = optCutOff.publicESPN)
confusionMatrix(publicESPN$Drafted, predicted.publicESPN, optCutOff.publicESPN)


## Tree

ind = sample(2, nrow(publicESPN), replace=TRUE, prob=c(0.8,.2))
trainData = publicESPN
#testData = publicESPN[ind==2,]

trainData=publicESPN

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

train.prune= prune.misclass(tree.train, best = 3)
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


finalpublicESPNtree=train.prune
finalpublicESPNtree

plot(finalpublicESPNtree)
text(finalpublicESPNtree, pretty = 0)
title(main = "PublicESPN Classification Tree")
publicESPN.pred = predict(finalpublicESPNtree,trainData3, type= "class")
cm3=table(predicted = publicESPN.pred, actual = trainData3$DraftStatus)
cm3
(sum(diag(cm3)))/sum(cm3)
cm3[2,2]/(cm3[2,1]+cm3[2,2])
