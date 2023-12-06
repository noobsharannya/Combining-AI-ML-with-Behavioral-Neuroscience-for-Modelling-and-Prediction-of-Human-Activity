# Read Data
data_EI_rf <- read.csv("EI.csv", header = T)
str(data_EI)
data_EI_rf$Output <- as.factor(data_EI_rf$Output)
str(data_EI)
table(data_EI_rf$Output)
pie(table(data_EI_rf$Output), labels = c("Champion", "Master", "Rookie"), main =  "Pie Chart of Distribution")
# 1 - Champion, 2 - Master, 3 - Rookie

# Data Partition
set.seed(123)
ind <- sample(2,nrow(data_EI_rf), replace = T, prob = c(0.7,0.3))
train <- data_EI_rf[ind ==1,]
test <- data_EI_rf[ind ==2,]

# Random Forest
install.packages("randomForest")
library("randomForest")
set.seed(222)
rf <- randomForest(Output~., data = train, 
                   ntree =300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)
rf$confusion

# Prediction and Confusion Matrix
install.packages("caret")
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Output)

# Prediction and confusion Matrix with Test data
p2 <- predict(rf,test)
confusionMatrix(p2, test$Output)

# Error Rate
plot(rf)

# Tune Random Forest
t <-tuneRF(train[,-10], train[,10],
        stepFactor = 0.5,
        plot = TRUE,
        ntreeTry = 200,
        trace = TRUE,
        improve = 0.05)

# Number of Nodes for trees
hist(treesize(rf),
     main = "Number of Nodes for the Tree",
     col = "grey")

# Varible Inportance
varImpPlot(rf, sort = T, main = "variable Importance")
importance(rf)
varUsed(rf)

# Partial depedence Plot
partialPlot(rf, train, Q_1, "1")

# Extract single tree from the forest
getTree(rf, 1, labelVar = TRUE)

# Mutli-dimensional scaling plot of proximity matrix
MDSplot(rf,train$Output)
