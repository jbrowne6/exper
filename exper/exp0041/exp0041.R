# ---
# title: "Accuracy as number of samples grows"
# author: "James Browne"
# date: "May 16 2017"
#output: html_document
# ---

library(ggplot2)
library(reshape)
library(scales)
library(plyr)
library(randomForest)
library(ranger)
library(xgboost)
library(rerf)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

runRerF <- FALSE#TRUE
runXG <- FALSE#TRUE
runRF <- TRUE
runRanger <- FALSE#TRUE
nTimes <- 10
num_trees <-  100
median_time <- NA
num.threads <- 20
samp_size <-c(.05, .10, .20, .40, .80) 
data_percent <- NULL
Alg <- NULL
NameResults <- NULL
Results <- NULL
data <- data.frame()
sink("out.log")
for(zp in samp_size){
    ######################################################
    ##########   MNIST ###################################
    #Size of the labels is 1 whereas everything else is 4
    #Open and position the image file
    image_block <- file("../../data/ubyte/train-images-idx3-ubyte", "rb")
    q <- readBin(image_block, integer(), n=1, endian="big")
    num_images <- readBin(image_block, integer(), n=1, endian="big")
    num_col <- readBin(image_block, integer(), n=1, endian="big")
    num_row <- readBin(image_block, integer(), n=1, endian="big")

    #Open and position the label file
    label_block = file("../../data/ubyte/train-labels-idx1-ubyte", "rb")
    q <- readBin(label_block, integer(), n=1, endian="big")
    num_labels <- readBin(label_block, integer(), n=1, endian="big")

    X <- readBin(image_block, integer(), n=num_images*num_col*num_row, size=1, signed=FALSE)
    X <- matrix(X, ncol=num_col*num_row, byrow=TRUE)

    Y <- as.numeric(readBin(label_block, integer(), n=num_labels, size=1, signed=FALSE)+1)

    X_choose <- NULL
    for(zq in unique(Y)){
        X1 <- sample(1:length(Y[Y==zq]), zp*length(Y[Y==zq]), replace=FALSE)
        X_choose <- c(X_choose, X1)
    }
    X <- X[X_choose,]
    Y <- Y[X_choose]

    close(image_block)
    close(label_block)

    image_block <- file("../../data/ubyte/t10k-images-idx3-ubyte", "rb")
    q <- readBin(image_block, integer(), n=1, endian="big")
    num_images <- readBin(image_block, integer(), n=1, endian="big")
    num_col <- readBin(image_block, integer(), n=1, endian="big")
    num_row <- readBin(image_block, integer(), n=1, endian="big")

    #Open and position the label file
    label_block = file("../../data/ubyte/t10k-labels-idx1-ubyte", "rb")
    q <- readBin(label_block, integer(), n=1, endian="big")
    num_labels <- readBin(label_block, integer(), n=1, endian="big")

    Xt <- readBin(image_block, integer(), n=num_images*num_col*num_row, size=1, signed=FALSE)
    Xt <- matrix(Xt, ncol=num_col*num_row, byrow=TRUE)

    Yt <- as.numeric(readBin(label_block, integer(), n=num_labels, size=1, signed=FALSE)+1)

    close(image_block)
    close(label_block)

    gc()

    ##########################################################
    print(paste("starting MNIST RERF: ", zp,"\n"))
    if(runRerF){
        ptm_hold <- NA
        for (i in 1:nTimes){
            gc()

            forest <- RerF(X,Y, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.threads, seed=sample(1:100000,1) )
            predictions <- Predict(Xt, forest, num.cores = num.threads)
            error.rate <- mean(predictions != Yt)
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RerF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("MNIST",nTimes))
    }

    ##########################################################
    print(paste("starting XGBoost RERF: ", zp,"\n"))
    if(runXG){
        num_classes <- length(unique(Y))
        train <- apply(X,2,as.numeric)
        label <- Y-1
        ptm_hold <- NA
        for (i in 1:nTimes){
            gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob", nrounds=num_trees,num_class=num_classes, nthread=num.threads)
            testS <- apply(Xt,2,as.numeric)
            testlabel <- Yt-1

            pred <- predict(forest, testS) 
            pred <- matrix(pred, ncol=num_classes, byrow=TRUE) 
            pred_labels <- max.col(pred) - 1
            error.rate <- mean(pred_labels != testlabel)

            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("XGBoost",nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("MNIST",nTimes))
    }

    ###########################################################
    print(paste("starting RF RERF: ", zp,"\n"))
    if(runRF){
        Yrf<-as.factor(as.character(Y))
        ptm_hold <- NA
        for (i in 1:nTimes){
            gc()
            forest <- randomForest(X,Yrf, ntree=num_trees)
            pred <- predict(forest, Xt)
            error.rate <- mean(pred != as.factor(as.character(Yt)))
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("MNIST",nTimes))
    }

    ###########################################################
    print(paste("starting MNIST Ranger: ", zp,"\n"))
    if(runRanger){
        X <- cbind(X,Y)
        colnames(X) <- as.character(1:ncol(X))

        ptm_hold <- NA
        for (i in 1:nTimes){
            gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.threads, classification=TRUE)
            colnames(Xt) <- as.character(1:ncol(Xt))
            pred <- predict(forest,Xt)
            error.rate <- mean(pred$predictions != Yt)
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("Ranger", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("MNIST",nTimes))
    }


    ################################################################
    ##############  HIGGS  ##########################################
    mydata <- read.csv(file="../../data/higgs/training.csv", header=TRUE, sep=",")
    X <- as.matrix(mydata[,2:31])
    Y <- as.numeric(mydata[,33])
    #mydata <- read.csv(file="../../data/higgs/test.csv", header=TRUE, sep=",")
    #Xt <- as.matrix(mydata[,2:32])
    #Yt <- as.numeric(mydata[,33])
    mydata <- NA
    gc()


    #############################################################
    print(paste("starting Higgs RERF: ", zp,"\n"))
    if(runRerF){
        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            gc()
            forest <- RerF(Xtr,Ytr, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.threads,seed=sample(1:100000,1))
            for( q in 1:num_trees){
                m <- which(is.na(forest$trees[[q]]$ClassProb))
                forest$trees[[q]]$ClassProb[m] <- .5
            }
            predictions <- Predict(Xte, forest, num.cores =num.threads)
            error.rate <- mean(predictions != Yte)
            ptm_hold[i] <- error.rate
        }

        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RerF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("Higgs",nTimes))
    }

    #############################################################
    print(paste("starting Higgs XGBoost: ", zp,"\n"))
    if(runXG){
        num_classes <- length(unique(Y))
        train <- apply(X,2,as.numeric)
        label <- Y-1

        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            train <- apply(Xtr,2,as.numeric)
            label <- Ytr-1
            gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob",nrounds=num_trees,max_depth=30000, num_class=num_classes, nthread=num.threads)
            testS <- apply(Xte,2,as.numeric)
            testlabel <- Yte-1
            pred <- predict(forest, testS) 
            pred <- matrix(pred, ncol=num_classes, byrow=TRUE)
            pred_labels <- max.col(pred) - 1
            error.rate <- mean(pred_labels != testlabel)

            ptm_hold[i] <-error.rate 
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("XGBoost",nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("Higgs",nTimes))
    }

    ##########################################################
    print(paste("starting higgs RF: ", zp,"\n"))
    if(runRF){

        #Yrf<-as.factor(as.character(Y))
        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            Yrf<-as.factor(as.character(Ytr))

            gc()
            forest <- randomForest(Xtr,Yrf, ntree=num_trees)
            pred <- predict(forest, Xte)
            error.rate <- mean(pred != as.factor(as.character(Yte)))
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("Higgs",nTimes))
    }

    ##########################################################
    print(paste("starting higgs ranger: ", zp,"\n"))
    if(runRanger){
        #X <- cbind(X,Y)
        #colnames(X) <- as.character(1:ncol(X))


        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            Xtr <- cbind(Xtr, Ytr)
            colnames(Xtr) <- as.character(1:ncol(Xtr))

            gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(Xtr)), data = Xtr, num.trees = num_trees, num.threads =num.threads, classification=TRUE)
            ptm <- proc.time()
            colnames(Xte) <- as.character(1:ncol(Xte))
            pred <- predict(forest,Xte)

            error.rate <- mean(pred$predictions != Yte)
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("Ranger", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("Higgs",nTimes))
    }


    ################################################################
    ################### p53 ######################################
    mydata <- read.csv(file="../../data/p53.csv", header=TRUE, sep=",")
    X <- as.matrix(mydata[,1:(ncol(mydata)-1)])
    Y <- as.numeric(mydata[,ncol(mydata)])
    mydata <- NA
    gc()

    print(paste("starting p53 rerf: ", zp,"\n"))
    if(runRerF){
        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            gc()
            forest <- RerF(Xtr,Ytr, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.threads,seed=sample(1:100000,1))
            predictions <- Predict(Xte, forest, num.cores = num.threads)
            error.rate <- mean(predictions != Yte)
            ptm_hold[i] <- error.rate
        }

        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RerF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("p53",nTimes))
    }

    ############################################################
    print(paste("starting p53 XGBoost: ", zp,"\n"))
    if(runXG){
        num_classes <- length(unique(Y))

        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            train <- apply(Xtr,2,as.numeric)
            label <- Ytr-1

            gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob",nrounds=num_trees,max_depth=30000, num_class=num_classes, nthread=num.threads)
            testS <- apply(Xte,2,as.numeric)
            testlabel <- Yte-1
            pred <- predict(forest, testS) 
            pred <- matrix(pred, ncol=num_classes, byrow=TRUE)
            pred_labels <- max.col(pred) - 1
            error.rate <- mean(pred_labels != testlabel)
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("XGBoost",nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("p53",nTimes))
    }

    ############################################################
    print(paste("starting p53 rf: ", zp,"\n"))
    if(runRF){

        #Yrf<-as.factor(as.character(Y))
        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            Yrf<-as.factor(as.character(Ytr))

            gc()
            forest <- randomForest(Xtr,Yrf, ntree=num_trees)
            pred <- predict(forest, Xte)
            error.rate <- mean(pred != as.factor(as.character(Yte)))
            ptm_hold[i] <-error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RF", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("p53",nTimes))
    }

    #############################################################
    print(paste("starting p53 rf: ", zp,"\n"))
    if(runRanger){
        ptm_hold <- NA
        for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)

            Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

            X1 <- sample(1:length(X1), (zp/.8)*length(X1), replace=FALSE)
            X2 <- sample(1:length(X2), (zp/.8)*length(X2), replace=FALSE)

            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

            Xtr <- cbind(Xtr, Ytr)
            colnames(Xtr) <- as.character(1:ncol(Xtr))
            gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(Xtr)), data = Xtr, num.trees = num_trees, num.threads =num.threads, classification=TRUE)
            colnames(Xte) <- as.character(1:ncol(Xte))
            pred <- predict(forest,Xte)
            error.rate <- mean(pred$predictions != Yte)
            ptm_hold[i] <-error.rate
        }
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("Ranger", nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("p53",nTimes))
    }


}
##############################################################
#################   Print ###################################

ress1<-data.frame(Dataset=as.factor(NameResults), Alg=Alg, Results = Results, data_Percent = as.factor(data_percent))

#ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="accuracy.Rdata")
#save(data_percent, file="dp.Rdata")

pWidth = 300
pHeight = 300
tWidth = pWidth * .05
tHeight = .13 * pHeight

cols <- c("Ideal"="#000000", "RerF"="#009E73", "XGBoost"="#E69F00", "Ranger"="#0072B2", "RF"="#CC79A7")

png(file="exp0041.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(data_Percent, Results,color = Alg)) + geom_point(position=position_jitterdodge(dodge.width=0.5)) + leg + labs(title="Error Rates", x="Ratio of Data Used for Training", y="Error Rate ")+ facet_grid(Dataset ~ .)+ scale_color_manual(values=cols))
dev.off()
sink()
