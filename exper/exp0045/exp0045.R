#title: "Test speed-up and scale-up of R-Rerf Iteration 8"
#author: "James Browne"
#date: "May 16 2017"

library(ggplot2)
library(reshape)
library(scales)
library(plyr)
library(randomForest)
library(ranger)
library(xgboost)
library(rerf)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

runRerF <- TRUE
runPack <- TRUE
runXG <- TRUE
runRF <- FALSE#TRUE
runRanger <- TRUE
nTimes <- 2
num_trees <- 128 #2048# 100
median_time <- NA
num.threads <- 64
data <- data.frame()

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
Xt <- matrix(X, ncol=num_col*num_row, byrow=TRUE)

Yt <- as.numeric(readBin(label_block, integer(), n=num_labels, size=1, signed=FALSE)+1)

close(image_block)
close(label_block)

gc()

NameResults <- "MNIST"
Results <- NULL
Alg <- NULL

if(runRerF){
	ptm_hold <- NA
for (i in 1:nTimes){
		gc()
	forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores = num.threads)
	
		ptm <- proc.time()
		predictions <- Predict(Xt, forest, num.cores =1 )
		error.rate <- mean(predictions != Yt)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "RerF")

	for (i in 1:nTimes){
	forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores = num.threads)

	PackForest(X,Y, forest)
		gc()
		ptm <- proc.time()
		predictions <- PackPredict(Xt)
		error.rate <- mean(predictions != Yt)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "Pack")

}

#create impossible dataset
if(runXG){
	num_classes <- length(unique(Y))
	train <- apply(X,2,as.numeric)
	label <- Y-1
	ptm_hold <- NA
for (i in 1:nTimes){
		gc()
	forest <- xgboost(data=train, label=label, objective="multi:softmax", nrounds=num_trees,num_class=num_classes, nthread=num.threads, max_depth=150)

	testS <- apply(Xt,2,as.numeric)
	testlabel <- Yt-1
	
		ptm <- proc.time()

		pred <- predict(forest, testS) 
		pred <- matrix(pred, ncol=num_classes, byrow=TRUE) 
		pred_labels <- max.col(pred) - 1
		error.rate <- mean(pred_labels != testlabel)

		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "XGBoost")
}

#create impossible dataset
if(runRF){
	Yrf<-as.factor(as.character(Y))
	ptm_hold <- NA
for (i in 1:nTimes){
		gc()
	forest <- randomForest(X,Yrf, ntree=num_trees)
		ptm <- proc.time()
		pred <- predict(forest, Xt)
		error.rate <- mean(pred != as.factor(as.character(Yt)))
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "RF")

}

#create impossible dataset
if(runRanger){
	X <- cbind(X,Y)
	colnames(X) <- as.character(1:ncol(X))

	ptm_hold <- NA
for (i in 1:nTimes){
		gc()
	forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.threads, classification=TRUE)
	colnames(Xt) <- as.character(1:ncol(Xt))
			ptm <- proc.time()
		pred <- predict(forest,Xt,num.threads = 1)
		error.rate <- mean(pred$predictions != Yt)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "Ranger")
}

data <- rbind(data, Results)

mydata <- read.csv(file="../../data/higgs/training.csv", header=TRUE, sep=",")
X <- as.matrix(mydata[,2:31])
Y <- as.numeric(mydata[,33])
#mydata <- read.csv(file="../../data/higgs/test.csv", header=TRUE, sep=",")
#Xt <- as.matrix(mydata[,2:32])
#Yt <- as.numeric(mydata[,33])
mydata <- NA
gc()

NameResults <- c(NameResults, "Higgs")
Results <- NULL
Alg <- NULL

if(runRerF){
	#create impossible dataset

	ptm_hold <- NA
	for (i in 1:nTimes){
		gc()

		X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
		X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
		Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
		Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
		Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
		Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])


		forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores = num.threads)
		#  for( q in 1:num_trees){
		#m <- which(is.na(forest$trees[[q]]$ClassProb))
		#forest$trees[[q]]$ClassProb[m] <- .5
		#            }
		ptm <- proc.time()
		predictions <- Predict(Xte, forest, num.cores =1)
		error.rate <- mean(predictions != Yte)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}

	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "RerF")

	for (i in 1:nTimes){
		gc()
		X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
		X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
		Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
		Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
		Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
		Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])


		forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores = num.threads)

	PackForest(X,Y, forest)

		ptm <- proc.time()
		predictions <- PackPredict(Xte)
		error.rate <- mean(predictions != Yte)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "Pack")
}

#create impossible dataset
if(runXG){
	num_classes <- length(unique(Y))
	train <- apply(X,2,as.numeric)
	label <- Y-1

	ptm_hold <- NA
	for (i in 1:nTimes){
		X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
		X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
		Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
		Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
		Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
		Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
		train <- apply(Xtr,2,as.numeric)
		label <- Ytr-1
		gc()

	forest <- xgboost(data=train, label=label, objective="multi:softmax", nrounds=num_trees,num_class=num_classes, nthread=num.threads, max_depth=150)
		testS <- apply(Xte,2,as.numeric)
		testlabel <- Yte-1
		ptm <- proc.time()
		pred <- predict(forest, testS) 
		pred <- matrix(pred, ncol=num_classes, byrow=TRUE)
		pred_labels <- max.col(pred) - 1
		error.rate <- mean(pred_labels != testlabel)

		ptm_hold[i] <-(proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "XGBoost")
}

#create impossible dataset
if(runRF){

	#Yrf<-as.factor(as.character(Y))
	ptm_hold <- NA
	for (i in 1:nTimes){
		X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
		X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
		Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
		Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
		Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
		Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
		Yrf<-as.factor(as.character(Ytr))

		gc()
		forest <- randomForest(Xtr,Yrf, ntree=num_trees)
		ptm <- proc.time()
		pred <- predict(forest, Xte)
		error.rate <- mean(pred != as.factor(as.character(Yte)))
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "RF")
}

#create impossible dataset
if(runRanger){
	#X <- cbind(X,Y)
	#colnames(X) <- as.character(1:ncol(X))


	ptm_hold <- NA
	for (i in 1:nTimes){
		X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
		X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
		Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
		Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])
		Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
		Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])
		Xtr <- cbind(Xtr, Ytr)
		colnames(Xtr) <- as.character(1:ncol(Xtr))

		gc()
		forest <- ranger(dependent.variable.name = as.character(ncol(Xtr)), data = Xtr, num.trees = num_trees, num.threads =num.threads, classification=TRUE)
		ptm <- proc.time()
		colnames(Xte) <- as.character(1:ncol(Xte))
		pred <- predict(forest,Xte,num.threads = 1)

		error.rate <- mean(pred$predictions != Yte)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "Ranger")
}


data <- rbind(data, Results)

ress1 <- cbind(NameResults,data)
colnames(ress1) <- c("Dataset", Alg)

#ress1<-data.frame(Dataset=as.factor(dataset), Time_Sec=median_time, Line_Type = as.factor(line_type))
ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="testTime.Rdata")

png(file="exp0045.png")
print(g <- ggplot(ress1, aes(Dataset, value,color = variable, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+ leg + labs(title="Test Times Single Core", x="Dataset", y="Test Time (s) ", subtitle=paste("")))
dev.off()
