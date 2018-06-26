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
runXG <- FALSE
runRF <- FALSE
runRanger <- FALSE
nTimes <- 2
num_trees <- 128
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
Xt <- matrix(Xt, ncol=num_col*num_row, byrow=TRUE)

Yt <- as.numeric(readBin(label_block, integer(), n=num_labels, size=1, signed=FALSE)+1)

close(image_block)
close(label_block)

gc()

NameResults <- "MNIST"
Results <- NULL
Alg <- NULL

if(runRerF){
	ptm_hold <- NA

	forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=num_trees, seed=sample(1:10000,1),mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), num.cores = num.threads)
for (i in 1:nTimes){
		gc()
	
		ptm <- proc.time()
#		predictions <- Predict(Xt, forest, num.cores =1 )
#		error.rate <- mean(predictions != Yt)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
#	Results <- c(Results, median(ptm_hold))
#	Alg <- c(Alg, "RerF")

predictions <- NA
	PackForest(X,Y, forest)
	for (i in 1:nTimes){

		gc()
		ptm <- proc.time()
		for(rNum in 1:length(Yt)){
		predictions[rNum] <- PackPredict(Xt[rNum,,drop=FALSE],1)
}
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
	pred <- NA

	forest <- xgboost(data=train, label=label, objective="multi:softmax", nrounds=num_trees,num_class=num_classes, nthread=num.threads, max_depth=150)
for (i in 1:nTimes){
		gc()

	testS <- apply(Xt,2,as.numeric)
	testlabel <- Yt-1
	
		ptm <- proc.time()
		for(rNum in 1:length(Yt)){
		pred[rNum] <- predict(forest, testS[rNum,,drop=FALSE]) 
		}	
		error.rate <- mean(pred == testlabel)

		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "XGBoost")
}

#create impossible dataset
if(runRF){
	Yrf<-as.factor(as.character(Y))
	ptm_hold <- NA
	forest <- randomForest(X,Yrf, ntree=num_trees)
for (i in 1:nTimes){
		gc()
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
pred <- NA
	ptm_hold <- NA

	forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.threads, classification=TRUE)
for (i in 1:nTimes){
		gc()
	colnames(Xt) <- as.character(1:ncol(Xt))
			ptm <- proc.time()
		for(rNum in 1:length(Yt)){
		pred[rNum] <- predict(forest,Xt[rNum,,drop=FALSE],num.threads = 1)
		}
		error.rate <- mean(pred$predictions != Yt)
		ptm_hold[i] <- (proc.time() - ptm)[3]
	}
	Results <- c(Results, median(ptm_hold))
	Alg <- c(Alg, "Ranger")
}

data <- rbind(data, Results)


ress1 <- cbind(NameResults,data)
colnames(ress1) <- c("Dataset", Alg)



ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="testTime.Rdata")

png(file="exp0046.png")
print(g <- ggplot(ress1, aes(Dataset, value,color = variable, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+ leg + labs(title="Test Times Single Core", x="Dataset", y="Test Time (s) ", subtitle=paste("")))
dev.off()
