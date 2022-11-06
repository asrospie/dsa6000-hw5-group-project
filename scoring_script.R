func1 <- function(newdata) {
    newdata$ArrDelay <- NULL
    newdata$DepDelay <- NULL
    newdata$UniqueCarrier <- ifelse(newdata$UniqueCarrier == "AA", 0,
        ifelse(newdata$UniqueCarrier == "DL", 1, 2))

    weights <- list(
        intercept = -0.8654318,
        Month = -0.1216587,
        UniqueCarrier = -0.1363635,
        SchedElapsedTime = 0.0110078,
        Distance = -0.0017499
    )

    n <- nrow(newdata)
    score <- numeric(n)

    for (i in 1:n) {
        score[i] <- # append(score,
             exp(weights$intercept + weights$Month * newdata[i, "Month"] +
                weights$UniqueCarrier * newdata[i, "UniqueCarrier"] +
                weights$SchedElapsedTime * newdata[i, "SchedElapsedTime"] +
                weights$Distance * newdata[i, "Distance"]) /
            (1 + exp(weights$intercept + weights$Month * newdata[i, "Month"] +
                weights$UniqueCarrier * newdata[i, "UniqueCarrier"] +
                weights$SchedElapsedTime * newdata[i, "SchedElapsedTime"] +
                weights$Distance * newdata[i, "Distance"]))
        # )
    }

    return(score)
}

func2 <- function(newdata) {
    score <- func1(newdata)
    label <- ifelse(score > 0.3, 1, 0)
    return(label)
}
