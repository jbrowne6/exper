library(ggplot2)
library(reshape)
 library(scales)
library(plyr)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

runRerF <- TRUE
runXG <- TRUE
runRF <- TRUE
runRanger <- TRUE
nTimes <- 1
num_trees <-  20
median_time <- NA
num.threads <- 20
data <- data.frame()
runMNIST <- FALSE
runHiggs <- FALSE
runP53 <- TRUE

if(runMNIST){
#### Loading the images and labels
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
image_block <- NULL
label_block <- NULL

gc()

NameResults <- "MNIST"
Results <- NULL
Alg <- NULL


########################################################################
#create impossible dataset
ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v1")
for (i in 1:nTimes){
    gc()
    forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
    predictions <- Predict(Xt, forest, num.cores = num.threads)
    error.rate <- mean(predictions != Yt)
    ptm_hold[i] <- error.rate
}
Results <- c(Results, ptm_hold)
Alg <- c(Alg, rep("RF", nTimes))
detach(package:rerf)

######################################################################
		ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v2")
		for (i in 1:nTimes){
    gc()
    forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
    predictions <- Predict(Xt, forest, num.cores = num.threads)
    error.rate <- mean(predictions != Yt)
    ptm_hold[i] <- error.rate
		}
        Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RFone",nTimes))
detach(package:rerf)

data <- rbind(data, Results)
}

if(runHiggs){
#### Loading Higgs
mydata <- read.csv(file="../../data/higgs/training.csv", header=TRUE, sep=",")
X <- as.matrix(mydata[,2:31])
Y <- as.numeric(mydata[,33])
mydata <- NA
gc()

NameResults <- c(NameResults, "Higgs")
Results <- NULL
Alg <- NULL

		ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v1")
		for (i in 1:nTimes){
X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

			gc()
forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
			for( q in 1:num_trees){
m <- which(is.na(forest$trees[[q]]$ClassProb))
forest$trees[[q]]$ClassProb[m] <- .5
            }
predictions <- Predict(Xte, forest, num.cores =num.threads)
     error.rate <- mean(predictions != Yte)
			ptm_hold[i] <- error.rate
		}
        
        Results <- c(Results, ptm_hold)
Alg <- c(Alg, rep("RF", nTimes))
detach(package:rerf)


		ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v2")
for (i in 1:nTimes){
X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

			gc()
	forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
	        for( q in 1:num_trees){
m <- which(is.na(forest$trees[[q]]$ClassProb))
forest$trees[[q]]$ClassProb[m] <- .5
            }
predictions <- Predict(Xte, forest, num.cores =num.threads)
     error.rate <- mean(predictions != Yte)
			ptm_hold[i] <- error.rate
		}
detach(package:rerf)
Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RFone",nTimes))

data <- rbind(data, Results)
}

if(runP53){

mydata <- read.csv(file="../../data/p53.csv", header=TRUE, sep=",")
X <- as.matrix(mydata[,1:(ncol(mydata)-1)])
Y <- as.numeric(mydata[,ncol(mydata)])
mydata <- NA
gc()

NameResults <- c(NameResults, "p53")
Results <- NULL
Alg <- NULL

		ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v1")
		for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
			gc()
	forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
			     error.rate <- mean(predictions != Yte)
			ptm_hold[i] <- error.rate
		}
        
        Results <- c(Results, ptm_hold)
Alg <- c(Alg, rep("RF", nTimes))
detach(package:rerf)



		ptm_hold <- NA
library(rerf, lib.loc="~/R/mult/v2")
for (i in 1:nTimes){
            X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
            X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
            Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
            Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
            Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
			gc()
	forest <- RerF(Xtr,Ytr,min.parent =1, max.depth=0, trees=num_trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = num.threads, stratify=TRUE)
predictions <- Predict(Xte, forest, num.cores = num.threads)
     error.rate <- mean(predictions != Yte)
			ptm_hold[i] <- error.rate
		}
		
Results <- c(Results, ptm_hold)
        Alg <- c(Alg, rep("RFone",nTimes))
detach(package:rerf)


data <- rbind(data, Results)
}

ress1 <- cbind(NameResults,data)
colnames(ress1) <- c("Dataset", Alg)

ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="accuracy.Rdata")

pWidth = 600
pHeight= 600
png(file="exp0043.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(Dataset, value,color = variable)) + geom_point(position=position_jitterdodge(dodge.width=0.5))+ leg + labs(title="Error Rates", x="Dataset", y="Average Error Rate ", subtitle=paste("")))
dev.off()
