#Choose Experiment to load
Experiment <- "RF2"# "RF", "R/L", "R/S" 
#Remove midkine and prolacting boolean
NoProt <- FALSE
# Set target specification
targetSpec <- .98
# Set number of trees
trees <- 98#1000
set.seed(53)
possExp <- c( "R/L", "R/S","RF")

#    for(protType in 1:2){
#        if(protType == 1){
#            NoProt <- FALSE
#        } else{
#            NoProt <- TRUE
#        }

if(Experiment == "RF"){
    print("Random Forest No Constraints")
    library(rerf, lib.loc="~/R/mult/v1")
}else if(Experiment == "R/L"){
    print("Random Forest R/L Constraint")
    library(rerf, lib.loc="~/R/mult/v2")
}else if(Experiment == "R/S"){
    print("Random Forest R/S Constraint")
    library(rerf, lib.loc="~/R/mult/v3")
}else if(Experiment == "RF2"){
    print("Random Forest RF w/Constraint")
    library(rerf, lib.loc="~/R/mult/v4")
}else{
    stop("Unknown Experiment")
}
library(ggplot2)

print(paste("trees= ", trees, ", targetSpec= ", targetSpec))

# Possible Data manipulation Functions
rangeNormalize <- function(x){(x-min(x))/(max(x)-min(x))}
rangeTest <- function(x,y){
    minNon <- sort(unique(x))[2]
    y[y<minNon] <- 0
    y <- (y-min(x))/(max(x)-min(x))
}

print("Loading Data")
#Load data and prepare data
load(file="CSeek.RData")
Y <- as.numeric(X[,2])+1
X <- as.matrix(X[,-c(1,2)])

#Remove proteins if specified by NoProt
if(NoProt){
    print("Removing Midkine and Prolactin")
    X <- X[,-c(23,28)]
}else{

    print("Using all proteins")
}


#initialize variables
predRate <- NA
specificity <- NA
sensitivity <- NA
cutOffAcc <- NA
PossibleCutOffs <- NULL

if(TRUE){
print("Determining Possible Class Decision Values")
for(i in 1:10){
    #Make 80 % training and 20% testing sets, ensure stratification
    X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
    X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
    Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
    Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
    Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
    Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
    YteLength1<- length(Y[Y==1][-X1])

    if(length(Xtr[,1])+length(Xte[,1]) != length(X[,1])){
        stop("Observation total is not correct")
    }
    # Grow forest       
    # This forest is single core to help in debugging
    #    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "binary", rho = 1/ncol(X)), num.cores=1, rank.transform=TRUE)
    # This forest is grown multicore and should be used if debugging is unnecessary
    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "binary", rho = 1/ncol(X)), rank.transform=TRUE)

    # columns of predictionTotals represent the trees and rows represent the observations
    predictionTotals <- Predict(Xte,forest,Xtrain= Xtr, aggregate.output = FALSE)

    # Loop through the possible decision values (i.e. the ratio of 
    # trees voting no cancer.), store decision values which give specification > targetSpec
    for(cutOff in 1:length(unique(rowMeans(predictionTotals)))){
        # pred2 is the decision value for sorted observation #, sorted observation # = cutOff
        pred2 <- (rowMeans(predictionTotals)>= sort(unique(rowMeans(predictionTotals)))[cutOff])+1
        # Acc is the specification based on decision value for sorted observation
        Acc <- sum(pred2[1:YteLength1] == 1)/YteLength1
        # if decision value is greater than targetSpec then the remainder of the decision values
        # will also be greater than targetSpec.  Store this minimum targetSpec through to the highest.
        if(Acc >= targetSpec){
            PossibleCutOffs <- c(PossibleCutOffs,sort(unique(rowMeans(predictionTotals)))[cutOff:length(unique(rowMeans(predictionTotals)))])
            cutOffAcc[i] <- sort(unique(rowMeans(predictionTotals)))[cutOff]
            break
        }
        # if no decision value gives spec >= targetSpec then set cutOffAcc to maximum value
        cutOffAcc[i] <- 2
    }
}
}else{
PossibleCutOffs<- seq(1.85,1.95,.01)
PossibleCutOffs<- 1.89
}

print("Calculating specificity and sensitivity based on possible decision values")
# the number of decision values
numCuts <- length(unique(PossibleCutOffs))
# the unique decision values
PossibleCutOffs <- unique(PossibleCutOffs)
# create holder matrices to store results
maxSens <- 0
treeDepth <- NA

trees <- c(100,500,1000)
#trees <- 98
mpV <- c(1,2,3,4)
mdV <- c(4,5,6,7,8)
mVal <- ceiling(sqrt(ncol(X))/2)
mtryV <- c(mVal, mVal*2, mVal*3, mVal*4)
#mtryV <- c(14,15,16,17,18)

tVal <- 1/ncol(X)/2 
rhV <- c(tVal,tVal*2,tVal*3,tVal*4,tVal*5,tVal*6)

for(treeN in trees){
for(mp in mpV){
    for(md in mdV){
        for(mtry in mtryV){
            for(rh in rhV){
                    # Run the experiment 10 times
                    SpecMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)
                    SensMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)
                    PredMatrix <- matrix(10*numCuts, nrow=numCuts, ncol=10)

                    set.seed(53)
                    for(i in 1:10){
                        #Make 80 % training and 20% testing sets, ensuring stratification
                        X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
                        X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
                        Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
                        Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
                        Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
                        Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

                        # Grow forest       
                        #  forest <- RerF(Xtr,Ytr,min.parent =3, max.depth=8, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))/2*3), random.matrix = "rf", rho =3/ncol(X)/2 ))
                        forest <- RerF(Xtr,Ytr,min.parent =mp, max.depth=md, trees=treeN, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =mtry, random.matrix = "binary", rho =rh ), rank.transform=TRUE)
                        predictionTotals <- Predict(Xte,forest,Xtrain= Xtr, aggregate.output = FALSE)

                        for(z in 1:numCuts){ 
                            pred <- (rowMeans(predictionTotals)>= PossibleCutOffs[z])+1
                            predRate[i] <-  sum(pred == Yte)/nrow(Xte)
                            # withC are the noncancer predictions
                            withC <- which(Yte == 1)
                            #specificity is the proportion of noncancers correctly identified
                            specificity[i] <- sum(pred[withC] == Yte[withC])/length(withC)
                            sensitivity[i] <- sum(pred[-withC] == Yte[-withC])/length(Yte[-withC])
                            PredMatrix[z,i] <- predRate[i]
                            SpecMatrix[z,i] <- specificity[i]
                            SensMatrix[z,i] <- sensitivity[i]


                            # store the results for decision value z and run i
                            if(FALSE){
                            if(predRate[i] > specificity[i] | predRate[i] < sensitivity[i]){
                                print(predRate)
                                print(specificity)
                                print(sensitivity)
                                stop("impossible prediction rate detected")
                            }
                            }
                        }
                    }
                    for(z in 1:numCuts){
                        specificity2 <- median(SpecMatrix[z,])
                        sensitivity2 <- median(SensMatrix[z,])
                        if(specificity2 > .99){
                            if(sensitivity2 > maxSens){
                                maxSens <-sensitivity2
                                maxParent <- mp 
                                maxDepth <- md
                                maxmtry <- mtry
                                maxrh <- rh
                                maxCV <- PossibleCutOffs[z]
                                maxtree <- treeN
                                print(paste("max sens= ", maxSens, " maxParent= ", mp, "maxDepth= ", md, "mtry= ", mtry, "rh= ", rh, "cutOff= ", maxCV, " trees= ", treeN))
print(summary(SpecMatrix[z,]))
                       print( summary(SensMatrix[z,]))

                            }
                        }
                    }

            }
        }
    }
}
}
