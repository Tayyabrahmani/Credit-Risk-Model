loan_train <- read.csv('train.csv')
loan_test <- read.csv('test.csv')

str(loans)
summary(loans)
loans$credit.policy = as.factor(loans$credit.policy)
str(loans)

library(caTools)
set.seed(100)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
modLog = glm(not.fully.paid ~. -annualincome, data=train, family="binomial")
summary(modLog)

modLog2 = glm(not.fully.paid ~ purpose + int.rate + installment + log.annual.inc + inq.last.6mths + pub.rec, data=train, family="binomial")
summary(modLog2)

test$predicted.risk = predict(modLog2, newdata=test, type="response")
table(test$not.fully.paid, as.numeric(test$predicted.risk >= 0.5))