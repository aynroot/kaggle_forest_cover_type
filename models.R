library(caret)

set.seed(42)
data <- read.csv("train.csv")
inTrain <- createDataPartition(y=data$Cover_Type, list=F, p=0.7)
train <- data[inTrain,]
test <- data[-inTrain,]

# Remove Id column
train <- train[, -1]
test <- test[, -1]
train$Cover_Type <- as.factor(train$Cover_Type)
test$Cover_Type <- as.factor(test$Cover_Type)

make_submission(model) {

}

evaluate(model, test_data) {
    cm <- confusionMatrix(data = predict(model, newdata=test_data))), reference = test_data$Cover_Type)
    print(cm)
    #cm_norm <- cm$table / rowSums(cm$table) * 100
    #cm_norm
    #cm$byClass
}

kNNfit <- function(train, test) {
    ctrl <- trainControl(method="cv", number = 7)
    knnFit <- train(Cover_Type ~ ., data=train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 10)
}