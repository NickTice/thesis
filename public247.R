# Loading the necessary packages
library('rvest')
library('stringr')
library('xlsx')



x=file.choose()
final247=read.xlsx(x,1)
public247=subset(final247, subset = (Private==0))
public247=subset(public247, select = -c(Column1, Year, Player, HighSchool))
names(public247) = c("Grade", "Position", "State", "Height", "Weight", "Drafted", "Private", "Enrollment", "AllBoys", "Minority", "EconomicDis", "Graduation")

  
# Drops categorical variables and reorders them so the draft column is last
public247=subset(public247, select = -c(Private, AllBoys))
public247=na.omit(public247)
public247$EconomicDis=as.numeric(public247$EconomicDis)

public247$Northeast=0
for (x in which(public247$State %in% c("ME","NH","MA","RI","CT","VT","NY","PA","NJ", "DC","DE", "MD"))) {
  public247[x,11]=1
}

public247$South=0
for (x in which(public247$State %in% c("WV","VA","KY","TN","NC","SC","GA","AL","MS","AR","FL","LA"))){
  public247[x,12]=1
}

public247$Southwest=0
for (x in which(public247$State %in% c("TX","OK", "NM", "AZ","UT","NV"))){
  public247[x,13]=1
}

public247$Midwest=0
for (x in which(public247$State %in% c("OH","IN","MI","IL","MO","WI","MN","IA","KS","NE","SD","ND"))){
  public247[x,14]=1
}

public247$West=0
for (x in which(public247$State %in% c("CO","WY","MT","ID","WA","OR","CA","AK","HI"))){
  public247[x,15]=1
}


# Dummy coding for positions

public247$OFF=0
for (x in which(public247$Position %in% c("DUAL", "PRO","OC","OG","OT", "RB","APB","WR","TE"))){
  public247[x,16]=1
}

public247$DEF=0
for (x in which(public247$Position %in% c("SDE","WDE","DT","ILB","OLB","S","CB"))){
  public247[x,17]=1
}

public247=subset(public247, select = -c(State, Position))
public247=public247[c(1:3,5:15,4)]

library(bestglm)
public247AIC=bestglm(public247, IC="AIC", family = binomial, TopModels = 10 )
public247AIC$BestModels
public247BIC$BestModels

fit1=glm(Drafted~ Grade + Height + Graduation + Southwest + West + OFF, data = public247, family = "binomial")
fit2=glm(Drafted~ Grade + Height + EconomicDis + Southwest + West + OFF, data = public247, family = "binomial")
fit3=glm(Drafted~ Grade + Height + EconomicDis + Southwest + OFF, data = public247, family = "binomial")
fit4=glm(Drafted~ Grade + Height + Southwest + OFF, data = public247, family = "binomial")
fit5=glm(Drafted~ Grade + Height + Minority + Southwest+ OFF, data = public247, family = "binomial")
fit.test=glm(Drafted~ Grade + Height + Southwest + West + OFF, data = public247, family = "binomial")

compareGLM(fit.null,fit1,fit2,fit3,fit4,fit5,fit.test)

anova(fit.null,fit4,fit2,fit3,fit1,fit5,fit.test,test="Chisq")

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit.test)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)
BIC(fit.test)

# Fit 4 has lowest BIC. Similar AIC to others. In the ten best models based on AIC, Grade, Height, Southwest, and OFF
# are included in every model.


# Hosmer-Lemoshow Test
# p-value over 0.05 indicates a good fit.
library(ResourceSelection)
hoslem.test(public247$Drafted, fitted(fit1), g=7)
hoslem.test(public247$Drafted, fitted(fit2), g=7)
hoslem.test(public247$Drafted, fitted(fit3), g=4)
hoslem.test(public247$Drafted, fitted(fit4), g=5)
hoslem.test(public247$Drafted, fitted(fit5), g=6)


anova(fit1, "Chisq")
anova(fit2, "Chisq")
anova(fit3, "Chisq")
anova(fit4, "Chisq")
anova(fit5, "Chisq")
anova(fit.test, "Chisq")

library(lmtest)
library(survey)
# Likelihood ratio test
lrtest(fit1,fit4)

#Wald Test
regTermTest(fit4, "OFF")

anova(fit4,fit1, test="Chisq")



# Creates predicted variables
predicted1 <- predict(fit1, public247, type="response")
predicted2 <- predict(fit2, public247, type="response")
predicted3 <- predict(fit3, public247, type="response")
predicted4 <- predict(fit4, public247, type="response")
predicted5 <- predict(fit5, public247, type="response")

library(InformationValue)
# provides ways to find the optimal cutoff to improve the prediction of 1's, 0's, 
# both 1's and 0's and o reduce the misclassification error.
optCutOff1 <- optimalCutoff(public247$Drafted, predicted1)[1]
optCutOff2 <- optimalCutoff(public247$Drafted, predicted2)[1]
optCutOff3 <- optimalCutoff(public247$Drafted, predicted3)[1]
optCutOff4 <- optimalCutoff(public247$Drafted, predicted4)[1]
optCutOff5 <- optimalCutoff(public247$Drafted, predicted5)[1]

# Misclassification error is the percentage mismatch of predcited vs actuals, irrespective 
# of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(public247$Drafted, predicted1, threshold = optCutOff1)
misClassError(public247$Drafted, predicted2, threshold = optCutOff2)
misClassError(public247$Drafted, predicted3, threshold = optCutOff3)
misClassError(public247$Drafted, predicted4, threshold = optCutOff4)
misClassError(public247$Drafted, predicted5, threshold = optCutOff5)

# traces the percentage of true positives accurately predicted by a given logit model as the 
# prediction probability cutoff is lowered from 1 to 0. Greater area under the better.
plotROC(public247$Drafted, predicted1)
plotROC(public247$Drafted, predicted2)
plotROC(public247$Drafted, predicted3)
plotROC(public247$Drafted, predicted4)
plotROC(public247$Drafted, predicted5)


sensitivity(public247$Drafted, predicted1, threshold = optCutOff1)
specificity(public247$Drafted, predicted1, threshold = optCutOff1)
precision(public247$Drafted, predicted1, threshold = optCutOff1)
confusionMatrix(public247$Drafted, predicted5)

sensitivity(public247$Drafted, predicted2, threshold = optCutOff2)
specificity(public247$Drafted, predicted2, threshold = optCutOff2)
precision(public247$Drafted, predicted2, threshold = optCutOff2)


sensitivity(public247$Drafted, predicted3, threshold = optCutOff3)
specificity(public247$Drafted, predicted3, threshold = optCutOff3)
precision(public247$Drafted, predicted3, threshold = optCutOff3)


sensitivity(public247$Drafted, predicted4, threshold = optCutOff4)
specificity(public247$Drafted, predicted4, threshold = optCutOff4)
precision(public247$Drafted, predicted4, threshold = optCutOff4)
confusionMatrix(public247$Drafted, predicted4, optCutOff4)


sensitivity(public247$Drafted, predicted5, threshold = optCutOff5)
specificity(public247$Drafted, predicted5, threshold = optCutOff5)
precision(public247$Drafted, predicted5, threshold = optCutOff5)

# Final model
public247.log=glm(Drafted~ Grade + Height + Southwest + OFF, data = public247, family = "binomial")
predicted.public247 <- predict(public247.log, public247, type="response")

optCutOff.public247 <- optimalCutoff(public247$Drafted, predicted.public247)


misClassError(public247$Drafted, predicted.public247, threshold = optCutOff.public247)

plotROC(public247$Drafted, predicted.public247)


sensitivity(public247$Drafted, predicted.public247, threshold = optCutOff.public247)
specificity(public247$Drafted, predicted.public247, threshold = optCutOff.public247)
precision(public247$Drafted, predicted.public247, threshold = optCutOff.public247)
confusionMatrix(public247$Drafted, predicted.public247, optCutOff.public247)



# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/#computing-penalized-logistic-regression
x=model.matrix(Drafted~.,public247)[,-1]
y=public247$Drafted
# alpha 1 for lasso 0 for ridge.
# In penalized regression, you need to specify a constant lambda to adjust the amount of the coefficient shrinkage. The best lambda for your data, can be defined as the lambda that minimize the cross-validation prediction error rate. 
# This can be determined automatically using the function cv.glmnet().

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model=glmnet(x, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
# Make prediction on test data
probabilities <- model %>% predict(newx = x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- public247$Drafted
coef(model)
mean(predicted.classes == observed.classes)

full.model <- glm(Drafted ~., data = public247, family = binomial)
# Make predictions
probabilities <- full.model %>% predict(public247, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
# Model accuracy
observed.classes <- public247$Drafted
mean(predicted.classes == observed.classes)

library(tree)
#SVM


ind = sample(2, nrow(public247), replace=TRUE, prob=c(0.8,.2))
trainData = public247
#testData = public247[ind==2,]


DraftStatus = ifelse(trainData$Drafted==1, "Drafted", "Undrafted")
trainData=data.frame(trainData, DraftStatus)
DraftStatus = ifelse(testData$Drafted==1, "Drafted", "Undrafted")
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

finalpublic247tree=train.prune
finalpublic247tree

plot(finalpublic247tree)
text(finalpublic247tree, pretty = 0)
title(main = "Public247 Classification Tree")
public247.pred = predict(finalpublic247tree,trainData1, type= "class")
cm4=table(predicted = public247.pred, actual = trainData1$DraftStatus)
cm4
(sum(diag(cm4)))/sum(cm4)
cm4[2,2]/(cm4[2,1]+cm4[2,2])

kjj