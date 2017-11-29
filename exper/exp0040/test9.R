library(rerf)
rangeNormalize <- function(x){(x-min(x))/(max(x)-min(x))}
rangeTest <- function(x,y){
    minNon <- sort(unique(x))[2]
    y[y<minNon] <- 0
    y <- (y-min(x))/(max(x)-min(x))
}

load(file="ForJushua.RData")
set.seed(42)
X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
Y<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1
#calculate amount to bag in order to fully devolop trees
targetSpec <- .99
print("")
print("**************************************************")
print("***** Starting Multiple Runs for Accuracy ********")
print("Removing Positive Samples until .99 Sensitivity Reached")
predRate <- NA
specificity <- NA
sensitivity <- NA
trees <- 100
cutOffAcc <- NA
for(i in 1:10){
    #Make 80 % training and 20% testing sets
    X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
    X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
    # X3 <- sample(1:length(X2), hold.out * length(X2), replace=FALSE)
    Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
    Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
    Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
    Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
    YteLength1<- length(Y[Y==1][-X1])

    # Grow forest       
    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))
    predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

    for(cutOff in 1:length(unique(rowMeans(predictionTotals)))){
        pred2 <- (rowMeans(predictionTotals)>= sort(unique(rowMeans(predictionTotals)))[cutOff])+1#new
        Acc <- sum(pred2[1:YteLength1] == 1)/YteLength1
        if(Acc >= targetSpec){
            cutOffAcc[i] <- sort(unique(rowMeans(predictionTotals)))[cutOff]
            break
        }
        cutOffAcc[i] <- 2
    }
}

for(i in 1:10){
    #Make 80 % training and 20% testing sets
    X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
    X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
    # X3 <- sample(1:length(X2), hold.out * length(X2), replace=FALSE)
    Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
    Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
    Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
    Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

    # Grow forest       
    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))
    predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

    pred <- (rowMeans(predictionTotals)>= median(cutOffAcc))+1
    predRate[i] <-  sum(pred == Yte)/nrow(Xte)
    withC <- which(Yte == 1)
    specificity[i] <- sum(pred[withC] == Yte[withC])/length(withC)
    sensitivity[i] <- sum(pred[-withC] == Yte[-withC])/length(Yte[-withC])
}

print(summary(predRate))
print(summary(specificity))
print(summary(sensitivity))

print(predRate)
print(specificity)
print(sensitivity)

