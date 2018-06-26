rf <- FALSE
if(rf){
library(rerf, lib.loc="~/R/mult/v1")
}else{
#library(rerf, lib.loc="~/R/mult/v2")
library(rerf, lib.loc="~/R/mult/v3")
}
library(ggplot2)


rangeNormalize <- function(x){(x-min(x))/(max(x)-min(x))}
rangeTest <- function(x,y){
    minNon <- sort(unique(x))[2]
    y[y<minNon] <- 0
    y <- (y-min(x))/(max(x)-min(x))
}

load(file="ForJushua.RData")
set.seed(42)
X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
NoProt <- TRUE
if(NoProt){
    X <- X[,-c(23,28)]
}
Y<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1
#calculate amount to bag in order to fully devolop trees
targetSpec <- .98
print("")
print("**************************************************")
print("***** Starting Multiple Runs for Accuracy ********")
print("Removing Positive Samples until .99 Sensitivity Reached")
predRate <- NA
specificity <- NA
sensitivity <- NA
trees <- 1000
cutOffAcc <- NA
#forest <- NA
PossibleCutOffs <- NULL
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
#    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores=1)
    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))
    predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

    for(cutOff in 1:length(unique(rowMeans(predictionTotals)))){
        pred2 <- (rowMeans(predictionTotals)>= sort(unique(rowMeans(predictionTotals)))[cutOff])+1#new
        Acc <- sum(pred2[1:YteLength1] == 1)/YteLength1
        if(Acc >= targetSpec){
            PossibleCutOffs <- c(PossibleCutOffs,sort(unique(rowMeans(predictionTotals)))[cutOff:length(unique(rowMeans(predictionTotals)))])
            cutOffAcc[i] <- sort(unique(rowMeans(predictionTotals)))[cutOff]
            break
        }
        cutOffAcc[i] <- 2
    }
}

numCuts <- length(unique(PossibleCutOffs))
PossibleCutOffs <- unique(PossibleCutOffs)
SpecMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)
SensMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)
PredMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)

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

   for(z in 1:numCuts){ 
    pred <- (rowMeans(predictionTotals)>= PossibleCutOffs[z])+1
    predRate[i] <-  sum(pred == Yte)/nrow(Xte)
    withC <- which(Yte == 1)
    specificity[i] <- sum(pred[withC] == Yte[withC])/length(withC)
    sensitivity[i] <- sum(pred[-withC] == Yte[-withC])/length(Yte[-withC])
    PredMatrix[z,i] <- predRate[i]
    SpecMatrix[z,i] <- specificity[i]
    SensMatrix[z,i] <- sensitivity[i]
   }
}

print(summary(predRate))
print(summary(specificity))
print(summary(sensitivity))

print(predRate)
print(specificity)
print(sensitivity)
pWidth = 600
pHeight = 600

specVec<-NA
sensVec <- NA
specificity2 <-NA
sensitivity2 <- NA
pred2 <- NA
for(z in 1:numCuts){
    specificity2[z] <- median(SpecMatrix[z,])
    sensitivity2[z] <- median(SensMatrix[z,])
    pred2[z] <- median(PredMatrix[z,])
}
ordSpec <- order(specificity2)

ress1 <- data.frame(specificity=specificity2[ordSpec], sensitivity=sensitivity2[ordSpec], cutval=PossibleCutOffs)


re1000RSp <- data.frame(type="1000RSp", specificity=specificity2[ordSpec], sensitivity=sensitivity2[ordSpec], cutval=PossibleCutOffs)
save(re1000RSp, file="1000RSp.Rdata")

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

if(rf){
lab <- labs(title="Random Forest 1000 Trees")
png(file="1000.png", width=pWidth, height=pHeight)
}else{
#lab <- labs(title="Random Forest 1000 Trees, (R/L)can > (R/L)non")
lab <- labs(title="Random Forest 1000 Trees, (R/S)can > (R/S)non", subtitle=paste("No Midkine, No Prolactin"))
png(file="RSp.png", width=pWidth, height=pHeight)
}
print(ggplot(ress1, aes(x=specificity, y = sensitivity)) + geom_line() + leg + lab)
dev.off()

#png(file="1000test.png", width=pWidth, height=pHeight)
#print(ggplot(re1000, aes(x=specificity, y = sensitivity)) + geom_line() + leg + lab)
#dev.off()





if(rf){
#lab <- labs(title="Random Forest 1000 Trees", subtitle=paste("Sensitivity and Specificity as a function of Cut Val\ncut val is proportion of trees that vote cancer"))
lab <- labs(title="Random Forest 1000 Trees", subtitle=paste("Sensitivity and Specificity as a function of Cut Val\ncut val is proportion of trees that vote cancer\nNo midkine, No Prolactin"))
}else{
lab <- labs(title="Random Forest 1000 Trees, (R/S)can > (R/S)non", subtitle=paste("Sensitivity and Specificity as a function of Cut Val\ncut val is proportion of trees that vote cancer"))
}
ress2 <- data.frame(type="Specificity", val=specificity2, cutval=PossibleCutOffs-1)
ress3 <- data.frame(type="Sensitivity", val=sensitivity2, cutval=PossibleCutOffs-1)
ress5 <- data.frame(type="Accuracy", val=pred2, cutval=PossibleCutOffs-1)
ress4 <- rbind(ress2,ress3,ress5)
png(file="exp0040-19.1000RSp.a.png", width=pWidth, height=pHeight)
print(ggplot(ress4, aes(x=cutval, y=val, group=type, colour=type)) + geom_line() + leg + lab)
dev.off()



