data_EI <- read.csv("EI.csv", header = T)
str(data_EI)
data_EI$Output <- as.factor(data_EI$Output)
str(data_EI)
data_EI$Output <- relevel(data_EI$Output, ref = "3")

# Develop Multinomial Logistic Regression Model
install.packages("nnet")
library(nnet)
mymodel <- multinom(Output~., data = data_EI)
summary(mymodel)

# Prediction
predict(mymodel,data_EI[c(4,100,201),], type = "prob")

# Misclassification Error
Conf_Matrix <- table(predict(mymodel), data_EI$Output)
print(Conf_Matrix)
# Rows are actual values

# Columns are predicted Values
mis_classi <- 1- sum(diag(Conf_Matrix))/sum(Conf_Matrix)
print(mis_classi)      

# 2 tailed z-test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
print(p)
