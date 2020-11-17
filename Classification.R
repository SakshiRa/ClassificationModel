getwd()
setwd("/Users/sakshirathi/Downloads/ITHplot-master")


training_data$label <- as.factor(training_data$label)
str(training_data)
test_data_OB$label <- as.factor(test_data_OB$label)
str(test_data_OB)

table(training_data$label)

table(test_data_OB$label)

options(digits = 2)

prop.table(table(training_data$label))

prop.table(table(test_data_OB$label))

library(AppliedPredictiveModeling)
library(caret)

transparentTheme(trans = .4)

featurePlot(x = training_data[,1:4], 
            y = training_data$label, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = training_data[,1:11], 
            y = training_data$label, 
            plot = "box",
            scales = list(y = list(relation = "free"),
                          x = list(rot = 90)),
            layout = c(4,1),
            ## Add a key at the top
            auto.key = list(columns = 2))


descrCor <-  cor(training_data[1:11])
descrCor

train_control <- trainControl(
  method = "cv",
  number = 4, 
  summaryFunction=twoClassSummary, # computes area under the ROC curve
  classProbs = TRUE ## required for scoring models using ROC
)


set.seed(1000)

rf_train <- train( label~., data = training_data,
                   method='rpart',
                   metric="ROC",
                   trControl = train_control)
rf_train
?train()
library(rpart.plot)
prp(rf_train$finalModel, box.palette = "Reds", tweak = 1.2, varlen = 20)

plot(varImp(rf_train))

rf_train$resample

rf_test <- predict(rf_train, test_data_OB) 
rf_test

str(rf_test)
str(test_data_OB$label)


# compare predicted outcome and true outcome
conf_matrix <- confusionMatrix(rf_test, test_data_OB$label)
conf_matrix$table

conf_matrix$byClass

## Can also spit out probability instead of predicted class
rf_test <- predict(rf_train, test_data_OB, type = "prob")
rf_test
rf_test <- rf_test[,1]

############ Plot ROC curve ############
## ROC curve
rf <- roc(test_data_OB$label,rf_test) ## pROC package
auc <- rf$auc
auc

plot(rf, col="blue",legacy.axes = TRUE)


