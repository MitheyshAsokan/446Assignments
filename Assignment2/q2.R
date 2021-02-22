library('glmnet')
setwd("~/446Assignments/Assignment2")
health <- read.csv("mental_health.csv")[,-1]

## 80% of the sample size
smp_size <- floor(0.80 * nrow(health))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(health)), size = smp_size)

train <- health[train_ind, ]
test <- health[-train_ind, ]

x   <- model.matrix(IsMentalHealthRelated ~ .,train)
y   <- train$IsMentalHealthRelated

fit.logreg <- glmnet(x,y,family="binomial")
head(coef(fit.logreg)[,1], 10)

fit.l1 <- glmnet(x,y,alpha=1,family="binomial", lambda = 0.002455181)
head(coef(fit.l1)[,1], 10)

fit.l2 <- glmnet(x,y,alpha=0,family="binomial", lambda = 0.02822862)
head(coef(fit.l2)[,1], 10)

# Model w.o Regularization
newdata_x   <- model.matrix(IsMentalHealthRelated ~ .,test)
probs <- predict(fit.logreg,newdata_x,type = "response")
preds <- ifelse(probs >= 0.5, 1, 0)
target <- ifelse(test$IsMentalHealthRelated == 1, 1, 0)
acc1 <- mean(preds == target)

# Model with L1 Regularization
newdata_x   <- model.matrix(IsMentalHealthRelated ~ .,test)
probs <- predict(fit.l1,newdata_x,type = "response")
preds <- ifelse(probs >= 0.5, 1, 0)
target <- ifelse(test$IsMentalHealthRelated == 1, 1, 0)
acc2 <- mean(preds == target)

# Model with L2 Regularization
newdata_x   <- model.matrix(IsMentalHealthRelated ~ .,test)
probs <- predict(fit.l2,newdata_x,type = "response")
preds <- ifelse(probs >= 0.5, 1, 0)
target <- ifelse(test$IsMentalHealthRelated == 1, 1, 0)
acc3 <- mean(preds == target)


### 2.4

sort(coef(fit.l1)[,1])