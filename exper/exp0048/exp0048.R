# ---

# title: "Accuracy vs allowed depth"
# author: "James Browne"
# date: "May 16 2017"
#output: html_document
# ---

library(ggplot2)
#library(reshape)
#library(scales)
#library(plyr)
library(rerf, lib.loc="installedRerF/")

data_summary <- function(data, varname, groupnames){
	require(plyr)
	summary_func <- function(x, col){
		c(mean = mean(x[[col]], na.rm=TRUE),
			sd = sd(x[[col]], na.rm=TRUE))
	}
	data_sum<-ddply(data, groupnames, .fun=summary_func,
									varname)
	data_sum <- rename(data_sum, c("mean" = varname))
	return(data_sum)
}

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

runRerF <- TRUE
nTimes <- 10
num_trees <- 500
num.threads <- 500

depthFileName <- "depth.Rdata"

minDepth <- 200
maxDepth <- 200

################################################################
################### cifar ######################################
if(TRUE){
	if(file.exists(depthFileName)){
		file.remove(depthFileName)
	}

	load(file="../../data/cifar10.Rdata")
	#X <- as.matrix(cifar_10[,~ncol(cifar_10)])
	X <- as.matrix(cifar_10[,-ncol(cifar_10)])
	Y <- as.numeric(cifar_10[,ncol(cifar_10)])
	cifar_10 <- NA
	gc()

	for(testDepth in minDepth:maxDepth){
		print(paste("starting cifar rerf: ", testDepth))
		for (i in 1:nTimes){
			X1 <- sample(1:length(Y), .8 *length(Y), replace=FALSE)

			Xte <- X[-X1,]
			Yte <- Y[-X1]

			Xtr <- X[X1,]
			Ytr <- Y[X1]

			gc()
			ptm <- proc.time()
			forest <- RerF(Xtr,Ytr, trees=num_trees, max.depth=testDepth, num.cores=num.threads,seed=sample(1:100000,1))
			predictions <- Predict(Xte, forest, num.cores = num.threads)
			pTime <- (proc.time() - ptm)[3]
		}
	}
	file.rename(depthFileName, "cifar10Depth.Rdata")
}

######################################################
##########   MNIST ###################################
#Size of the labels is 1 whereas everything else is 4
#Open and position the image file
if(TRUE){
	if(file.exists(depthFileName)){
		file.remove(depthFileName)
	}

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

	##########################################################
	for(testDepth in minDepth:maxDepth){
		print(paste("starting MNIST RERF: ", testDepth))
		for (i in 1:nTimes){

			ptm <- proc.time()
			forest <- RerF(X,Y, trees=num_trees, max.depth=testDepth, num.cores=num.threads, seed=sample(1:100000,1) )
			predictions <- Predict(Xt, forest, num.cores = num.threads)
			pTime <- (proc.time() - ptm)[3]
		}
	}
	file.rename(depthFileName, "mnistDepth.Rdata")
}

################################################################
##############  HIGGS  ##########################################
if(TRUE){

	if(file.exists(depthFileName)){
		file.remove(depthFileName)
	}

	mydata <- read.csv(file="../../data/higgs/training.csv", header=TRUE, sep=",")
	X <- as.matrix(mydata[,2:31])
	Y <- as.numeric(mydata[,33])
	#mydata <- read.csv(file="../../data/higgs/test.csv", header=TRUE, sep=",")
	#Xt <- as.matrix(mydata[,2:32])
	#Yt <- as.numeric(mydata[,33])
	mydata <- NA

	gc()


	#############################################################
	for(testDepth in minDepth:maxDepth){
		print(paste("starting Higgs RERF: ", testDepth))
		for (i in 1:nTimes){
			X1 <- sample(1:length(Y[Y==1]), .8 *length(Y[Y==1]), replace=FALSE)
			X2 <- sample(1:length(Y[Y==2]), .8 *length(Y[Y==2]), replace=FALSE)
			Xte <- rbind(X[Y==1,][-X1,],X[Y==2,][-X2,])
			Yte <- c(Y[Y==1][-X1], Y[Y==2][-X2])

			Xtr <- rbind(X[Y==1,][X1,],X[Y==2,][X2,])
			Ytr <- c(Y[Y==1][X1], Y[Y==2][X2])

			gc()
			ptm <- proc.time()
			forest <- RerF(Xtr,Ytr, trees=num_trees,max.depth=testDepth, num.cores=num.threads,seed=sample(1:100000,1))
			for( q in 1:num_trees){
				m <- which(is.na(forest$trees[[q]]$ClassProb))
				forest$trees[[q]]$ClassProb[m] <- .5
			}
			predictions <- Predict(Xte, forest, num.cores =num.threads)
			pTime <- (proc.time() - ptm)[3]
		}
	}
	file.rename(depthFileName, "higgsDepth.Rdata")
}

##############################################################
#################   Print ###################################


#data.testAccuracy <- data_summary(data.test, varname="accuracy", groupnames=c("dataset", "depth"))

#cols <- c("higgs"="#000000", "mnist"="#009E73", "cifar10"="#E69F00")

#g <- ggplot(data=data.test, aes(x=depth, y=accuracy,color = dataset))
#g <- g + geom_line(size=1.5)
#g <- g + leg + labs(title="Accuracy vs Depth", x="Training Depth", y="Error Rate ")
#g <- g + scale_color_manual(values=cols)


#png(file="exp0048.png")
#print(g)
#dev.off()
