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

# split into train, valid sets
head(flights)
trainset <- sample(1:nrow(flights), 4000)
validset <- setdiff(1:nrow(flights), trainset)
train <- flights[trainset, ]
valid <- flights[validset, ]

# decide if normalization is needed

# choose features


# build model
library("MASS")
mdl <- glm(Canceled ~ ., data = train, family = "binomial")
predict(mdl, newdata = valid, type = "response")
summary(mdl)
