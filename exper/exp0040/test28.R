#Choose Experiment to load
Experiment <- "RF"# "RF", "R/L", "R/S" 
#Remove midkine and prolacting boolean
NoProt <- FALSE
# Set target specification
targetSpec <- .98
# Set number of trees
trees <- 98#1000
set.seed(53)
possExp <- c( "R/L", "R/S","RF")

    for(protType in 1:2){
        if(protType == 1){
            NoProt <- FALSE
        } else{
            NoProt <- TRUE
        }

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

        #initialize variables
        predRate <- NA
        specificity <- NA
        sensitivity <- NA
        cutOffAcc <- NA
        PossibleCutOffs <- NULL

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
            #    forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores=1)
            # This forest is grown multicore and should be used if debugging is unnecessary
            forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))

            # columns of predictionTotals represent the trees and rows represent the observations
            predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

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

        print("Calculating specificity and sensitivity based on possible decision values")
        # the number of decision values
        numCuts <- length(unique(PossibleCutOffs))
        # the unique decision values
        PossibleCutOffs <- unique(PossibleCutOffs)
        # create holder matrices to store results
        ParSize <- NA
        ParentSizes <- c(1,2,3,4,5,8)#,4,6,8,10)
        SpecMatrix <- matrix(10*numCuts*length(ParentSizes), nrow=numCuts*length(ParentSizes), ncol=10)
        SensMatrix <- matrix(10*numCuts*length(ParentSizes), nrow=numCuts*length(ParentSizes), ncol=10)
        PredMatrix <- matrix(10*numCuts*length(ParentSizes), nrow=numCuts*length(ParentSizes), ncol=10)

        numRun <- -1
        for(ps in ParentSizes){
            numRun <- numRun+1
            # Run the experiment 10 times
            for(i in 1:10){
                #Make 80 % training and 20% testing sets, ensuring stratification
                X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
                X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
                Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
                Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
                Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
                Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

                # Grow forest       
                forest <- RerF(Xtr,Ytr,min.parent =ps, max.depth=0, trees=trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))
                predictionTotals <- Predict(Xte,forest, aggregate.output = FALSE)

                for(z in 1:numCuts){ 
                    pred <- (rowMeans(predictionTotals)>= PossibleCutOffs[z])+1
                    predRate[i] <-  sum(pred == Yte)/nrow(Xte)
                    # withC are the noncancer predictions
                    withC <- which(Yte == 1)
                    #specificity is the proportion of noncancers correctly identified
                    specificity[i] <- sum(pred[withC] == Yte[withC])/length(withC)
                    sensitivity[i] <- sum(pred[-withC] == Yte[-withC])/length(Yte[-withC])
                    # store the results for decision value z and run i
                    PredMatrix[(numCuts*numRun)+z,i] <- predRate[i]
                    SpecMatrix[(numCuts*numRun)+z,i] <- specificity[i]
                    SensMatrix[(numCuts*numRun)+z,i] <- sensitivity[i]
                    ParSize[(numCuts*numRun)+z] <- ps
                    if(predRate[i] > specificity[i] | predRate[i] < sensitivity[i]){
                        stop("impossible prediction rate detected")
                    }
                }
            }
        }
        ParSize <- as.factor(ParSize)
        # Format results for printing
        specVec<-NA
        sensVec <- NA
        specificity2 <-NA
        sensitivity2 <- NA
        pred2 <- NA
        for(z in 1:(numCuts*length(ParentSizes))){
            specificity2[z] <- median(SpecMatrix[z,])
            sensitivity2[z] <- median(SensMatrix[z,])
            pred2[z] <- median(PredMatrix[z,])
        }
        # order results and place in data frame
        ordSpec <- order(specificity2)
        ress1 <- data.frame(specificity=specificity2[ordSpec], sensitivity=sensitivity2[ordSpec], cutval=PossibleCutOffs, ParentSize=ParSize[ordSpec])
#        if(length(unique(ParSize)) != 2){
#stop(paste("did not use right parsize. ", length(unique(ParSize))))
#        }

        ress2 <- data.frame(type="Specificity", val=specificity2, cutval=PossibleCutOffs-1)
        ress3 <- data.frame(type="Sensitivity", val=sensitivity2, cutval=PossibleCutOffs-1)
        ress5 <- data.frame(type="Accuracy", val=pred2, cutval=PossibleCutOffs-1)
        ress4 <- rbind(ress2,ress3,ress5)


        # Print results
        print(summary(predRate))
        print(summary(specificity))
        print(summary(sensitivity))

        print(predRate)
        print(specificity)
        print(sensitivity)


        ########################################################################
        # Setup chart parameters
        pWidth = 800
        pHeight = 800

        leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

        # Setup experiment specific graphing parameters
        if(Experiment == "RF" & NoProt){
            lab <- labs(title="Random Forest 1000 Trees without Midkine and Prolactin", subtitle="variable size minParent")
            png(file="1000MP_p.png", width=pWidth, height=pHeight)
            ##############
        }else if(Experiment == "RF" & !NoProt){
            lab <- labs(title="Random Forest 1000 Trees", subtitle="variable size minParent")
            png(file="1000MP.png", width=pWidth, height=pHeight)
            ##############
        }else if(Experiment == "R/L" & NoProt){
            lab <- labs(title="Random Forest 1000 Trees", subtitle="R/L, omit, variable size minParent")
            png(file="1000MP_RLp.png", width=pWidth, height=pHeight)
            ##############
        }else if(Experiment == "R/L" & !NoProt){
            lab <- labs(title="Random Forest 1000 Trees", subtitle="R/L, variable size minParent")
            png(file="1000MP_RL.png", width=pWidth, height=pHeight)
            ##############
        }else if(Experiment == "R/S" & NoProt){
            lab <- labs(title="Random Forest 1000 Trees", subtitle="R/S, omit, variable size minParent")
            png(file="1000MP_RSp.png", width=pWidth, height=pHeight)
            ##############
        }else if(Experiment == "R/S" & !NoProt){
            lab <- labs(title="Random Forest 1000 Trees", subtitle="R/S, variable size minParent")
            png(file="1000MP_RS.png", width=pWidth, height=pHeight)
        }
        ##############
        print(ggplot(ress1, aes(x=specificity, y = sensitivity, colour=ParentSize)) + geom_line(aes(linetype=ParentSize)) + leg + lab)
        dev.off()

    }

