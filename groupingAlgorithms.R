library(e1071)
library(DescTools)

variance <- function(values) {
  vMean <- mean(values)
  result <- (sum((values - vMean)^2))/(length(values) - 1)
  
  return(result)
}

columnVariances <- function(trainData) {
  variances <- c()
  
  for(i in 1:ncol(trainData)) {
    variances <- c(variances, variance(trainData[, i]))
  }
  
  return(variances)
}

normalizeData <- function(trainData) {
  means <- colMeans(trainData)
  variances <- columnVariances(trainData)
  
  normalizedData <- matrix(nrow = nrow(trainData), ncol = ncol(trainData))
  for(i in 1:nrow(trainData)) {
    for(j in 1:ncol(trainData)) {
      normalizedData[i, j] <- (trainData[i, j] - means[j]) / (variances[j])
    }
  }
  
  result <- list(data = normalizedData, means = means, variances = variances)
  
  return(result)
}

createModel <- function(trainData, centers = 7, algorithm = "k-means")  {
  normalizedData <- normalizeData(trainData)
  modelCenters <- NULL
  
  if("k-means" == algorithm) {
    kmeans.result <- kmeans(normalizedData$data, centers=centers, algorithm = "Lloyd" , iter.max = 10000) 
    modelCenters <- kmeans.result$centers
  } else {
    cmeans.result <- cmeans(normalizedData$data, centers = centers) 
    modelCenters <- cmeans.result$centers
  }
  
  result <- list(centers = modelCenters, means = normalizedData$means, variances = normalizedData$variances)
  return(result)
}

normalizeArguments <- function(arguments, model) {
  result <- matrix(ncol = ncol(arguments), nrow = 1)
  for(i in 1:ncol(arguments)) {
    result[1, i] <- (arguments[1, i] - model$means[i]) / (model$variances[i])
  }
  
  return(result)
}

predictValue <- function(model, arguments) {
  normalizedArgs <- normalizeArguments(arguments, model)
  
  minDist <- NULL
  
  for(i in seq.int(from = 1, to = 7)) {
    dist <- sqrt(rowSums((normalizedArgs - model$centers[i,])^2))
    if(is.null(minDist) || dist < minDist) {
      minDist <- dist
    } 
  }
  
  return(minDist)
}

countRocAuc <- function(anormalDist, normalDist, step = 0.005, stop = 20.0) {
  i <- 0.0
  
  truePos <- c()
  falsePos <- c()
  trueNeg <- c()
  falseNeg <- c()
  while (i <= stop) {
    truePos <- c(truePos, sum(anormalDist >= i))
    falseNeg <- c(falseNeg, sum(anormalDist < i))
    trueNeg <- c(trueNeg, sum(normalDist < i))
    falsePos <- c(falsePos, sum(normalDist >= i))
    
    i <- i + step
  }
  
  TPrate <- truePos / (truePos + falseNeg)
  FPrate <- falsePos / (falsePos + trueNeg)
  
  plot(x = FPrate, y=TPrate, type = "l")
  auc <- AUC(x = FPrate, y = TPrate)
  
  return(auc)
}

testModel <- function(anomalyData, normalData, model) {
  anomalyDistances <- c()
  for(i in 1:nrow(anomalyData)) {
    anomalyDistances <- c(anomalyDistances, predictValue(model, anomalyData[i, 1:8]))
  }
  
  normalDistances <- c()
  for(i in 1:nrow(normalData)) {
    normalDistances <- c(normalDistances, predictValue(model, normalData[i, 1:8]))
  }
  
  auc <- countRocAuc(anomalyDistances, normalDistances)
  
  return(auc)
}

data <- read.table("data/train.data", sep = ",", header = FALSE)
train <- data[, 1:8]
normalData <- read.table("data/noanomaly.data", sep = ",", header = FALSE)
anomalyData <- read.table("data/anomaly.data", sep = ",", header = FALSE)
normal <- normalData[, 1:8]
anomaly <- anomalyData[, 1:8]

model <- createModel(train)
auc <- testModel(anomalyData, normalData, model)