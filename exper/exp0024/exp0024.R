library(ranger)
    source("rfr_function.R")

sink("ranger.log")
trees <- 10#500
numT <- 3#0
numC <- 1#25

rowNum <- 100000
colNum <- 1000
classNum <- 4

X <- matrix(unlist(lapply((1:classNum*.5), function(x) runif(rowNum/classNum*colNum)+x)), nrow=rowNum, ncol=colNum, byrow=TRUE)
Y <- as.numeric(unlist(lapply(1:classNum, rep, times=rowNum/classNum)))


    ptmtrain_rerf <- NA
    memSize_rerf <- NA
for (i in 1:numT){
        initMem <- sum(gc(reset=TRUE)[9:10])
        ptm <- proc.time()
        forest<-rerf(X,Y,trees=trees, MinParent=1L, MaxDepth="inf", stratify=FALSE, COOB=FALSE, NumCores=numC,  options=c(ncol(X), ceiling(ncol(X)^.5),1L, 1/ncol(X)), seed = sample(1:10000,1)) 
        ptmtrain_rerf[i]<- (proc.time() - ptm)[3]
        memSize_rerf[i] <- sum(gc()[9:10]) - initMem
        rm(forest)
}
        print(paste("RerF median training time: ", median(ptmtrain_rerf)))
        print(paste("RerF median memory: ", median(memSize_rerf)))

if(exists("comp_err")){rm("comp_err", envir=.GlobalEnv)}
     if(exists("comp_errOOB")){rm("comp_errOOB", envir=.GlobalEnv)}
     if(exists("comp_rfr")){rm("comp_rfr", envir=.GlobalEnv)}
source("working_rfr.R")
    ptmtrain_rerfW <- NA
    memSize_rerfW <- NA
for (i in 1:numT){
        initMem <- sum(gc(reset=TRUE)[9:10])
        ptm <- proc.time()
        forest<-rfr(X,Y,trees=trees, MinParent=1L, MaxDepth="inf", stratify=FALSE, COOB=FALSE, NumCores=numC,  options=c(ncol(X), ceiling(ncol(X)^.5),1L, 1/ncol(X)), seed = sample(1:10000,1)) 
        ptmtrain_rerfW[i]<- (proc.time() - ptm)[3]
        memSize_rerfW[i] <- sum(gc()[9:10]) - initMem
        rm(forest)
}
        print(paste("RerFW median training time: ", median(ptmtrain_rerfW)))
        print(paste("RerFW median memory: ", median(memSize_rerfW)))


X <- cbind(X,Y)
colnames(X) <- as.character(1:ncol(X))
rm(Y)
gc()
 ptmtrain_ranger <- NA
    memSize_ranger <- NA
for (i in 1:numT){
        initMem <- sum(gc(reset=TRUE)[9:10])
        ptm <- proc.time()
            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = trees, num.threads = numC, classification=TRUE)
        ptmtrain_ranger[i]<- (proc.time() - ptm)[3]
        memSize_ranger[i] <- sum(gc()[9:10]) - initMem
        rm(forest)
}
print(paste("Ranger median training time: ", median(ptmtrain_ranger)))
        print(paste("Ranger median memory: ", median(memSize_ranger)))


#ptmtrain_rangerL <- NA
#    memSize_rangerL <- NA
#for (i in 1:numT){
#        initMem <- sum(gc(reset=TRUE)[9:10])
#        ptm <- proc.time()
#            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = trees, num.threads = numC, classification=TRUE, save.memory=TRUE)
#        ptmtrain_rangerL[i]<- (proc.time() - ptm)[3]
#        memSize_rangerL[i] <- sum(gc()[9:10]) - initMem
#        rm(forest)
#}
#print(paste("RangerLite median training time: ", median(ptmtrain_rangerL)))
#        print(paste("RangerLite median memory: ", median(memSize_rangerL)))

sink()
