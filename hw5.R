library(ggplot2)
library(leaps)
library("MASS")
# load data
flights <- read.table(file = "dsa6000-hw5-group-project/flight.csv",
    header = TRUE, sep = ",")

set.seed(42)

# Remove unnecessary features
flights$ArrDelay <- NULL
flights$DepDelay <- NULL

# Convert UniqueCarrier to numbers
# AA, DL, UA -> 0, 1, 2
flights$UniqueCarrier <- ifelse(flights$UniqueCarrier == "AA", 0,
    ifelse(flights$UniqueCarrier == "DL", 1, 2))

colnames(flights)

# choose features
boxplot(Month ~ Canceled, data = flights, xlab = "Canceled", ylab = "Month", main = "Month")
boxplot(DepartureTime ~ Canceled, data = flights, xlab = "Canceled", ylab = "DepartureTime", main = "DepartureTime")
boxplot(SchedElapsedTime ~ Canceled, data = flights, xlab = "Canceled", ylab = "SchedElapsedTime", main = "SchedElapsedTime")
boxplot(Distance ~ Canceled, data = flights, xlab = "Canceled", ylab = "Distance", main = "Distance")

ggplot(data = flights, mapping = aes(x = UniqueCarrier, y = Canceled, fill = UniqueCarrier))
flights_cor <- cor(flights)

# split into train, valid sets
head(flights)
trainset <- sample(1:nrow(flights), 4000)
validset <- setdiff(1:nrow(flights), trainset)
train <- flights[trainset, ]
valid <- flights[validset, ]

# forward or backward selection
log.mdl.fwd <- regsubsets(Canceled ~ ., data = train, method = "forward", nvmax = 20)
summary(log.mdl.fwd)
plot(summary(log.mdl.fwd)$bic, xlab = "# of Variables Forwards", ylab = "BIC", type = "l")
plot(summary(log.mdl.fwd)$cp, xlab = "# of Variables Forwards", ylab = "AIC", type = "l")
plot(summary(log.mdl.fwd)$adjr2, xlab = "# of Variables Forwards", ylab = "Adj. R2", type = "l")

log.mdl.bwd <- regsubsets(Canceled ~ ., data = train, method = "backward", nvmax = 20)
summary(log.mdl.bwd)
plot(summary(log.mdl.bwd)$bic, xlab = "# of Variables Backwards", ylab = "BIC", type = "l")
plot(summary(log.mdl.bwd)$cp, xlab = "# of Variables Backwards", ylab = "AIC", type = "l")
plot(summary(log.mdl.bwd)$adjr2, xlab = "# of Variables Backwards", ylab = "Adj. R2", type = "l")

# Test models with 2 and 4 variables based on AIC, BIC, Adj R^2 
train_scale <- train
train_scale$Month <- scale(train_scale$Month)
train_scale$Distance <- scale(train_scale$Distance)
log.mdl.four <- glm(Canceled ~ . - DepartureTime, data = train, family = binomial)
log.mdl.two <- glm(Canceled ~ Month + Distance, data = train_scale, family = binomial)

valid$Canceled[is.na(valid$Canceled)] <- 0
valid_scale <- valid
valid_scale$Month <- scale(valid_scale$Month)
valid_scale$Distance <- scale(valid_scale$Distance)
four.valid.log <- predict(log.mdl.four, newdata = valid, type = "response")
two.valid.log <- predict(log.mdl.two, newdata = valid_scale, type = "response")

# use .5 as cut off
choose_cutoff <- function(preds, acts, start = .5) {
    counter <- start
    best_acc <- 0
    best_cutoff <- start
    while (counter >= 0) {
        print(paste("Counter:", counter))
        test_preds <- ifelse(preds > counter, 1, 0)
        test_acc <- sum(test_preds == acts$Canceled) / nrow(acts)
        if (test_acc > best_acc) {
            best_acc <- test_acc
            best_cutoff <- counter
            print(paste("Best Acc.:", best_acc))
        }
        counter <- counter - .01
    }
    return(best_cutoff)
}

two_cutoff <- choose_cutoff(two.valid.log, valid, start = .5)
print(paste("Best Two Var. Cutoff:", two_cutoff))

four.valid.preds <- ifelse(four.valid.log > 0.5, 1, 0)
two.valid.preds <- ifelse(two.valid.log > 0.5, 1, 0)
four.valid.acc <- sum(four.valid.preds == valid$Canceled) / nrow(valid)
two.valid.acc <- sum(two.valid.preds == valid$Canceled) / nrow(valid)

print(paste("Two Var. Valid Acc. ::", two.valid.acc))
print(paste("Four Var. Valid Acc. ::", four.valid.acc))
# decide if normalization is needed
summary(log.mdl.four)
test_func1 <- func1(valid)
sum(test_func1 == four.valid.log) / nrow(valid)
test_func1
four.valid.log
