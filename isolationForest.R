library("isofor")
library("DescTools")

countRocAuc <- function(mod, outlierData, inlierData) {
  stop <- 1.0
  i <- 0.0
  step <- 0.001
  
  inlierPred <- predict(mod, inlierData)
  outlierPred <- predict(mod, outlierData)
  
  truePos <- c()
  falsePos <- c()
  trueNeg <- c()
  falseNeg <- c()
  while (i <= stop) {
    truePos <- c(truePos, sum(outlierPred >= i))
    falseNeg <- c(falseNeg, sum(outlierPred < i))
    trueNeg <- c(trueNeg, sum(inlierPred < i))
    falsePos <- c(falsePos, sum(inlierPred >= i))
    
    i <- i + step
  }
  
  TPrate <- truePos / (truePos + falseNeg)
  FPrate <- falsePos / (falsePos + trueNeg)
  
  plot(x = FPrate, y=TPrate, type = "l")
  auc <- AUC(x = FPrate, y = TPrate)
  
  return(auc)
}

data <- read.table("data/train.data", sep = ",", header = FALSE)
normalData <- read.table("data/noanomaly.data", sep = ",", header = FALSE)
anomalyData <- read.table("data/anomaly.data", sep = ",", header = FALSE)
normal <- normalData[, 1:8]
anomaly <- anomalyData[, 1:8]
train <- rbind(data[, 1:8], normal, anomaly)
mod <- iForest(X = train, 100, 32)
auc <- countRocAuc(mod, outlierData = anomaly, inlierData = normal)