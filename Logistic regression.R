#load data
train <- read.csv('Train_Old.csv')

#create training and validation data from given data
install.packages('caTools')
library(caTools)

set.seed(88)
split <- sample.split(train$Recommended, SplitRatio = 0.75)

#get training and test data
dresstrain <- subset(train, split == TRUE)
dresstest <- subset(train, split == FALSE)

#logistic regression model
model <- glm (Recommended ~ .-ID, data = dresstrain, family = binomial)
summary(model)
predict <- predict(model, type = 'response')
#confusion matrix
table(dresstrain$Recommended, predict > 0.5)
#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, dresstrain$Recommended)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
#plot glm
library(ggplot2)
ggplot(dresstrain, aes(x=Rating, y=Recommended)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)