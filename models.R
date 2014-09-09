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

make_submission(model, filename) {
    test_data <- read.csv("test.csv")
    test_data$Cover_Type <- rep(0, dim(test_data)[1])
    submission_data <- subset(test_data, select=c("Id", "Cover_Type"))
    test_data <- test_data[, -1]

    # add preprocessing
    test_data$Cover_Type <- as.integer(predict(model, newdata=test_data))
    submission_data$Cover_Type <- test_data$Cover_Type

    # save to csv file
    write.csv(file=filename, x=submission_data, row.names=FALSE)
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
    model <- train(Cover_Type ~ ., data=train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 10)
    model
}