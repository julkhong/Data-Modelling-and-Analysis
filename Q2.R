source("wrappers.R")
source("my.prediction.stats.R")
source("studio10.prediction.stats.R")
library(glmnet)
library(rpart)
library(randomForest)
library(kknn)
library(boot)


#2.1)
heart = read.csv("heart.train.ass3.2020.csv", header = TRUE)
tree.heart = rpart(HD ~., heart)
tree.heart
plot(tree.heart)
text(tree.heart, pretty = 12)
cv = learn.tree.cv(HD ~.,data=heart,nfolds=10,m=5000)
cv$best.tree
plot.tree.cv(cv)

#2.2)
plot(cv$best.tree)
text(cv$best.tree, pretty = 12)

#2.3)
cv$best.tree


#2.5)
glm.heart = glm(HD ~., data = heart , family = binomial )
glm.heart
glm.heart.bic = step(glm.heart, k = log(length(heart$HD)))
glm.heart.bic$coefficients

#2.6)
summary(glm.heart.bic)
glm.heart.bic$coefficients


#2.7)
heart.test = read.csv("heart.test.ass3.2020.csv", header = TRUE)
tree_pred = predict(cv$best.tree, heart.test)
tree_test = my.pred.stats(tree_pred[,2], heart.test$HD)


logistic_pred = predict(glm.heart.bic, heart.test, type ="response")
logistic_test = my.pred.stats(logistic_pred, heart.test$HD)

#2.8)
tree_pred.69 = predict(cv$best.tree, heart.test[69,])
tree.odds = tree_pred.69
tree.odds
logistic_pred.69 = predict(glm.heart.bic, heart.test[69,], type ="response")
log.odds = logistic_pred.69
log.odds


#2.9)
#Note : I've changed the studio10 prediction function name 
boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = as.character(fit$terms[[2]])
  rv = new.pred.stats(predict(fit,d,type="response"), d[,target], display =F)
  return(rv$auc)
}

mydata = heart[c("THALACH","OLDPEAK", "CP", "THAL", "CA", "HD")]
mydata
bs = boot(data=mydata, statistic=boot.auc, R=5000, formula=HD ~ .)
boot.ci(bs,conf=0.95,type="bca")
