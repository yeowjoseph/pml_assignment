library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
pml_train <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""));
pml_test <- read.csv("./pml-testing.csv", na.strings=c("NA","#DIV/0!",""));
dim(pml_train); dim(pml_test);
in_train <- createDataPartition(y=pml_train$classe, p=0.6, list=FALSE);
train_data <- pml_train[in_train, ];
test_data <- pml_train[-in_train, ];
dim(train_data); dim(test_data);
#nzv <- nearZeroVar(pml_train, saveMetrics = TRUE); #nzv;
non_nzv_train <- pml_train[, -nearZeroVar(pml_train)];
dim(non_nzv_train);
non_na_train <- non_nzv_train[, colSums(is.na(non_nzv_train)) < nrow(non_nzv_train) * 0.6];
non_na_train <- non_na_train[,7:length(colnames(non_na_train))]
dim(non_na_train);
str(non_na_train);
test_data <- test_data[names(non_na_train)];
dim(test_data);
str(test_data);
#fit <- randomForest(classe~., data=non_na_train, method="class");
#predictions <- predict(fit, test_data, type="class");
#confusionMatrix(predictions, test_data$classe);
pml_test <- pml_test[c(names(non_na_train)[1:52],'problem_id')];
#testing <- testingAll[ , which(names(testingAll) %in% names(trainData))]
#testing <- pml_test[ , which(names(pml_test) %in% names(non_na_train))]
dim(pml_test);
str(pml_test);
#predictions2 <- predict(fit, pml_test, type="class");
fit <- train(classe~., method="rf", data=non_na_train); fit;
predictions1 <- predict(fit, newdata=test_data);
confusionMatrix(predictions1, test_data$classe);
predict(fit, newdata=pml_test);