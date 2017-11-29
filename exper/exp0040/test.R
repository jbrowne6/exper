library(rerf)

fImportance <- function(forest,lengthX){
      fimp <- vector("integer",lengthX)
  
  for(z in 1:length(forest$trees)){
          lengthMatAstore <- length(forest$trees[[z]]$matAstore)
      eUse <- 1:lengthMatAstore%%2 == 1
      fimp <- fimp + tabulate(forest$trees[[z]]$matAstore[eUse], lengthX)
          }
          
    fimp
}


load(file="ForJushua.RData")

X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
Y<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1
#calculate amount to bag in order to fully devolop trees
targetSpe <- .99
print("")
print("**************************************************")
print("***** Starting Multiple Runs for Accuracy ********")
print("Removing Positive Samples until .99 Sensitivity Reached")
predRate <- NA
specificity <- NA
sensitivity <- NA
trees <- 1000
fimp <- vector("integer",ncol(X))
for(i in 1:100){
        #Make 80 % training and 20% testing sets
        X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
    X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
       # X3 <- sample(1:length(X2), hold.out * length(X2), replace=FALSE)
        Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
        Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

                # Grow forest       
                forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))

fimp <- fimp + fImportance(forest, ncol(X))
            predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)
            pred <- (rowMeans(predictionTotals)>= 1.75)+1
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

print(fimp)
print(order(fimp))
print(sort(fimp/max(fimp)))

overall <- data.frame(Prot_Num = order(fimp), Protein = colnames(X)[order(fimp)], Importance = sort(fimp/max(fimp)))
