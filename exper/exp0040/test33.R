#Choose Experiment to load
Experiment <- "R/L"# "RF", "R/L", "R/S" 
#Remove midkine and prolacting boolean
NoProt <- FALSE
# Set target specification
targetSpec <- .98
# Set number of trees
trees <- 98
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
load(file="ForJushua.RData")
X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
#Remove proteins if specified by NoProt
if(NoProt){
    print("Removing Midkine and Prolactin")
    X <- X[,-c(23,28)]
}else{

    print("Using all proteins")
}

Y<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1

specificity <- NA
sensitivity <- NA
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
    #                        maxParent=  3 maxDepth=  6 mtry=  16 rh=  0.0128205128205128 cutOff=  1.90816326530612
    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=5, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =14, random.matrix = "rf", rho = 3/ncol(X)/2))
    predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

        pred <- (rowMeans(predictionTotals)>= 1.89)+1
        # withC are the noncancer predictions
        withC <- which(Yte == 1)
        #specificity is the proportion of noncancers correctly identified
        specificity[i] <- sum(pred[withC] == Yte[withC])/length(withC)
        sensitivity[i] <- sum(pred[-withC] == Yte[-withC])/length(Yte[-withC])
}

print(summary(specificity))
print(summary(sensitivity))

