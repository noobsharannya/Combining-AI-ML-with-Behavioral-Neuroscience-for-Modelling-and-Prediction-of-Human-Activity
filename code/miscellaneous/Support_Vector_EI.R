# Read Data
data_EI_svm <- read.csv("Gail_data.csv", header = T)
str(data_EI_svm)
data_EI_svm <- data_EI_svm[,-1]
data_EI_svm$Output <- as.factor(data_EI_svm$Output)
str(data_EI_svm)
table(data_EI_svm$Output)
pie(table(data_EI_svm$Output), labels = c("Sell", "Buy"), main =  "Pie Chart of Distribution")

library(ggplot2)

#Support Vector machine
install.packages("e1071")
library(e1071)
ind <- sample(2, nrow(data_EI_svm), replace = T, prob = c(0.7,0.3))
train <- data_EI_svm[ind == 1, ]
test <- data_EI_svm[ind==2,]
dim(train)
dim(test)

anyNA(data_EI_svm)
?svm()

mymodel <- svm(Output~., data = train, kernel = "radial")
summary(mymodel)

# Confusion Matrix and Misclassification Error - training data
pred_train <- predict(mymodel, train)
length(pred_train)
tab <- table(Predicted = pred_train, Actual = train$Output)
tab
1 - sum(diag(tab))/sum(tab)

# Confusion Matrix and Misclassification Error - test data
pred_test <- predict(mymodel, test)
tab <- table(Predicted = pred_test, Actual = test$Output)#
tab
1 - sum(diag(tab))/sum(tab)

