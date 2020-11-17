getwd()
setwd("/Users/sakshirathi/ColorectalTumourClassification")


Data$label <- as.factor(Data$label)
str(Data)

dim(Data)

table(Data$label)

prop.table(table(Data$label))

library(AppliedPredictiveModeling)
library(caret)
library(pROC)

transparentTheme(trans = .4)

featurePlot(x = Data[,1:4], 
            y = Data$label, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = Data[,1:6], 
            y = Data$label, 
            plot = "box",
            scales = list(y = list(relation = "free"),
                          x = list(rot = 90)),
            layout = c(4,1),
            ## Add a key at the top
            auto.key = list(columns = 2))

descrCor <-  cor(Data[1:6])
descrCor

set.seed(1000)
# Split dataset into 80% training, and 20% test
train_index <- createDataPartition(Data$label, ## outcome
                                   p = 0.7, ## percentage of training samples
                                   list = FALSE ## show subsamples as matrix, not list
                                   # times = 10 ## This will create 10 different 80% subsamples
)

x.train <- Data[train_index,1:6] 

y.train <- Data$label[train_index]

x.test <- Data[-train_index,1:6]
y.test <- Data$label[-train_index]


train_control <- trainControl(
  method = "cv",
  number = 5, ## also try 10
  summaryFunction=twoClassSummary, # computes area under the ROC curve
  classProbs = TRUE ## required for scoring models using ROC
)

set.seed(1000)
rf_train <- train( x = x.train, y = as.factor(y.train),
                   method='rf',
                   metric="ROC", ## default accuracy
                   trControl = train_control)
rf_train
rf_train$resample

rf_test <- predict(rf_train, x.test) 
rf_test

# compare predicted outcome and true outcome
conf_matrix <- confusionMatrix(rf_test, y.test)
conf_matrix$table

conf_matrix$byClass

## Can also spit out probability instead of predicted class
rf_test <- predict(rf_train, x.test, type = "prob")
rf_test
rf_test <- rf_test[,1]

############ Plot ROC curve ############
## ROC curve
rf <- roc(y.test,rf_test) ## pROC package
auc <- rf$auc
auc

plot(rf, col="blue",legacy.axes = TRUE)
impVars <- varImp(rf_train)
ImpMeasure <- data.frame(impVars$importance)

round(ImpMeasure,2)
plot(varImp(rf_train))
