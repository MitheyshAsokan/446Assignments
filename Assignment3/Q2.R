library(ISLR)
library("rpart")
library("rattle")
library("caret")
library(randomForest)
library("MLmetrics")

heart <- read.csv("Heart.csv")[,-1]
setwd("~/446Assignments/Assignment3")

# Hitters$Salary <- log10(Hitters$Salary) #Make sure to uncomment on first iteration

## 70% of the sample size
smp_size_hitters <- floor(0.70 * nrow(Hitters))
smp_size_heart <- floor(0.70 * nrow(heart))

## set the seed to make your partition reproducible
set.seed(112)

## Split the Hitters data to 70/30
train_ind_hitters <- sample(seq_len(nrow(Hitters)), size = smp_size_hitters)
train_hitters <- Hitters[train_ind_hitters, ]
test_hitters <- Hitters[-train_ind_hitters, ]

## Split the heart data to 70/30
train_ind_heart <- sample(seq_len(nrow(heart)), size = smp_size_heart)
train_heart <- heart[train_ind_heart, ]
test_heart <- heart[-train_ind_heart, ]


## Q 2.2 Decision Trees for Regression
hitters.tree <- rpart(Salary ~ Hits + Years, data=train_hitters)
fancyRpartPlot(hitters.tree, caption="")

# Test Performance
preds <- predict(hitters.tree, test_hitters)
head(preds, 4)
ind <- !is.na(test_hitters$Salary) # Filter out the Na values in Salary
RMSE <- RMSE(preds[ind],test_hitters$Salary[ind])
n <- length(test_hitters$Salary[ind])
SSE <- (RMSE^2)*n

## Q 2.3. Decision Trees for Classification
heart.tree <- rpart(AHD ~ ., data=train_heart)
fancyRpartPlot(heart.tree, caption="")
preds_heart <- predict(heart.tree, test_heart, type = "class")
Accuracy(preds_heart[1:5],test_heart$AHD[1:5]) 
ConfusionMatrix(preds_heart, test_heart$AHD)

## Q 2.4 Bagging: Regression
train_hitters.bag <- train_hitters[complete.cases(train_hitters), ] # Only include data that's all filled
hitters.bag <- randomForest(Salary ~ . , data = train_hitters.bag, mtry = ncol(train_hitters.bag)-1) # all columns but the output
preds.bag <- predict(hitters.bag, test_hitters)
head(preds.bag, 4)
RMSE.bag <- RMSE(preds.bag[ind], test_hitters$Salary[ind])
n <- length(test_hitters$Salary[ind])
SSE.bag <- (RMSE.bag^2)*n

## Q 2.5 Bagging: Classification
train_heart.bag <- train_heart[complete.cases(train_heart), ]
train_heart.bag$AHD <- factor(train_heart.bag$AHD)
heart.bag <- randomForest(AHD ~ . , data = train_heart.bag)
preds_heart.bag <- predict(heart.bag, test_heart, type = "class")
Accuracy(preds_heart.bag[1:5],test_heart$AHD[1:5]) 
ConfusionMatrix(preds_heart.bag, test_heart$AHD)

## Q 2.6 Bagging: Regression
train_hitters_2.6 <- na.omit(train_hitters)
hitters.forest <- randomForest(Salary ~ . , data = train_hitters_2.6,mtry = 6, importance=T)
preds.forest <- predict(hitters.forest, test_hitters)
head(preds.forest, 4)
RMSE.forest <- RMSE(preds.forest[ind], test_hitters$Salary[ind])
n <- length(test_hitters$Salary[ind])
SSE.forest <- (RMSE.forest^2)*n




