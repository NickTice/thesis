# Loading the necessary packages
library('rvest')
library('stringr')
library('xlsx')
library(OptimalCutpoints)

# Reads in excel file. Necessary because manual imputation was made for the high school socioeconomic information.
excel=read.xlsx(file.choose(),1)
south247=subset(excel, select = -c(Column1, Year, HighSchool))
names(south247) = c("Grade", "Name", "Position", "State", "Height", "Weight", "Drafted", "Private", "Enrollment", "AllBoys", "Minority", "EconomicDis", "Graduation")

south247$Northeast=0
for (x in which(south247$State %in% c("ME","NH","MA","RI","CT","VT","NY","PA","NJ", "DC","DE", "MD"))) {
  south247[x,14]=1
}

south247$South=0
for (x in which(south247$State %in% c("WV","VA","KY","TN","NC","SC","GA","AL","MS","AR","FL","LA"))){
  south247[x,15]=1
}

south247$Southwest=0
for (x in which(south247$State %in% c("TX","OK", "NM", "AZ"))){
  south247[x,16]=1
}

south247$Midwest=0
for (x in which(south247$State %in% c("OH","IN","MI","IL","MO","WI","MN","IA","KS","NE","SD","ND"))){
  south247[x,17]=1
}

south247$West=0
for (x in which(south247$State %in% c("CO","WY","MT","ID","WA","OR","CA","AK","HI","UT","NV"))){
  south247[x,18]=1
}


# Dummy coding for positions
south247$ATH=0
for (x in which(south247$Position %in% "ATH")){
  south247[x,19]=1
}

south247$QB=0
for (x in which(south247$Position %in% c("DUAL", "PRO"))){
  south247[x,20]=1
}

south247$OL=0
for (x in which(south247$Position %in% c("OC","OG","OT"))){
  south247[x,21]=1
}

south247$RB=0
for (x in which(south247$Position %in% c("RB","APB"))){
  south247[x,22]=1
}

south247$REC=0
for (x in which(south247$Position %in% c("WR","TE"))){
  south247[x,23]=1
}

south247$DL=0
for (x in which(south247$Position %in% c("SDE","WDE","DT"))){
  south247[x,24]=1
}

south247$LB=0
for (x in which(south247$Position %in% c("ILB","OLB"))){
  south247[x,25]=1
}

south247$DB=0
for (x in which(south247$Position %in% c("S","CB"))){
  south247[x,26]=1
}

south247=subset(south247, subset = (South==1))
south247=subset(south247, subset = (Private==0))
south247=na.omit(south247)

south247=subset(south247, select = -c(Position, State, Private, AllBoys, Northeast, South, Southwest, Midwest, West))
south247=subset(south247, select = -c(Name))
south247=south247[c(1:3,5:16,4)]
south247=subset(south247, select = -c(ATH))


#######################################################3


library(bestglm)
south247AIC=bestglm(south247, IC="AIC", family = binomial, TopModels = 10 )
#south247BIC=bestglm(south247, IC="BIC", family = binomial, TopModels = 5 )
south247AIC$BestModels
south247BIC$BestModels

# Best fits based on the bestglm function
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

#### Final model ####

south247.log=glm(Drafted~ Grade + Height + Weight + REC, data = south247, family = "binomial")
predicted.south247 <- predict(south247.log, south247, type="response")

optCutOff.south247 <- optimalCutoff(south247$Drafted, predicted.south247)


misClassError(south247$Drafted, predicted.south247, threshold = optCutOff.south247)

plotROC(south247$Drafted, predicted.south247)


sensitivity(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
specificity(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
precision(south247$Drafted, predicted.south247, threshold = optCutOff.south247)
confusionMatrix(south247$Drafted, predicted.south247, optCutOff.south247)

predicted.south247=as.numeric(predicted.south247)
south247$Prob=predicted.south247
write.xlsx(south247, "247southProb.xlsx")


##########################################################################

library(tree)

traindata2 = south247

DraftStatus = ifelse(traindata2$Drafted==1, "Drafted", "Undrafted")
traindata2=data.frame(traindata2, DraftStatus)
traindata2=subset(traindata2, select = -Drafted)

tree.train=tree(DraftStatus ~ ., data = traindata2)
summary(tree.train)
plot(tree.train)
text(tree.train, pretty = 0)


train.pred=predict(tree.train, traindata2, type = "class")

cm1=table(predicted=train.pred, actual=traindata2$DraftStatus)
(sum(diag(cm1)))/sum(cm1)
cm1[2,2]/(cm1[2,1]+cm1[2,2])


train.cv = cv.tree(tree.train, FUN = prune.misclass)
min_idx=which.min(train.cv$dev)
train.cv$size[min_idx]

par(mfrow = c(2, 1))

plot(train.cv)
# better plot
plot(train.cv$size, train.cv$dev / nrow(traindata2), type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate")

train.prune= prune.misclass(tree.train, best =2 )
summary(train.prune)

plot(train.prune)
text(train.prune, pretty = 0)
title(main = "Pruned Classification Tree")

train.prune.pred = predict(train.prune, traindata2, type = "class")
cm3=table(predicted = train.prune.pred, actual = traindata2$DraftStatus)

(sum(diag(cm3)))/sum(cm3)
cm3[2,2]/(cm3[2,1]+cm3[2,2])


# Final Tree
finalsouth247tree=train.prune
plot(finalsouth247tree)
text(finalsouth247tree, pretty = 0)
title(main = "South247 Classification Tree")

south247.pred = predict(finalsouth247tree, traindata2, type = "class")
cm2=table(predicted = south247.pred, actual = traindata2$DraftStatus)
cm2
