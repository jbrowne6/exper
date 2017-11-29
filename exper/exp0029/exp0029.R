#---
# title: "Test speed-up and scale-up of R-Rerf Iteration 8"
#author: "James Browne"
# date: "May 16 2017"
#output: html_document
#---

library(ggplot2)
library(reshape)
 library(scales)
library(plyr)
library(randomForest)
library(ranger)
library(xgboost)
library(rerf)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))


nTimes <- 20
num_trees <- 25  
median_time <- NA
num.cores <- 25
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
Xr <- as.matrix(replicate(784,rnorm(60000)))
	Yr <- as.numeric(sample(1:10, 60000, replace=TRUE))
gc()

NameResults <- "MNIST"
Results <- NULL
Alg <- NULL


##################################  RerF #############################
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()

			forest <- RerF(X,Y, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.cores)
			ptm_hold[i] <- object.size(forest)
		}
        Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RerF")

################################# Xgboost ################################
num_classes <- length(unique(Y))
train <- apply(X,2,as.numeric)
label <- Y-1
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob", nrounds=num_trees,num_class=num_classes, nthread=num.cores)
			ptm_hold[i] <- object.size(forest)+ file.info("xgboost.model")$size
		}
        Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "XGBoost")


########################## random forest ########################
if(TRUE){
Yrf<-as.factor(as.character(Y))
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
			forest <- randomForest(X,Yrf, ntree=num_trees)
			ptm_hold[i] <- object.size(forest)
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RF")

}

############################### ranger ######################################
X <- cbind(X,Y)
colnames(X) <- as.character(1:ncol(X))

		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.cores, classification=TRUE)
			ptm_hold[i] <- object.size(forest)
		}
        Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "Ranger")


data <- rbind(data, Results)

############################  Higgs ##################################
mydata <- read.csv(file="../../data/higgs/training.csv", header=TRUE, sep=",")
X <- as.matrix(mydata[,2:32])
Y <- as.numeric(mydata[,33])
mydata <- NA
gc()

NameResults <- c(NameResults, "Higgs")
Results <- NULL
Alg <- NULL

################################## Rerf ################################
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
			forest <- RerF(X,Y, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.cores)
			ptm_hold[i] <- object.size(forest)
		}
        
        Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RerF")

############################# XGboost #############################
num_classes <- length(unique(Y))
train <- apply(X,2,as.numeric)
label <- Y-1

		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob",nrounds=num_trees,max_depth=30000, num_class=num_classes, nthread=num.cores)
			ptm_hold[i] <- object.size(forest)+ file.info("xgboost.model")$size
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "XGBoost")

############################### RF ################################
if(TRUE){

Yrf<-as.factor(as.character(Y))
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
			forest <- randomForest(X,Yrf, ntree=num_trees)
			ptm_hold[i] <- object.size(forest)
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RF")
}

################################### ranger ###########################
X <- cbind(X,Y)
colnames(X) <- as.character(1:ncol(X))


		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.cores, classification=TRUE)
			ptm_hold[i] <- object.size(forest)
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "Ranger")


data <- rbind(data, Results)


########################## p53 #####################################
mydata <- read.csv(file="../../data/p53.csv", header=TRUE, sep=",")
X <- as.matrix(mydata[,1:ncol(mydata)-1])
Y <- as.numeric(mydata[,ncol(mydata)])
mydata <- NA
gc()

NameResults <- c(NameResults, "p53")
Results <- NULL
Alg <- NULL

###################################### rerf ########################
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
			forest <- RerF(X,Y, trees=num_trees, bagging=.3, min.parent=1, max.depth=0, store.oob=TRUE, stratify=TRUE, num.cores=num.cores)
			ptm_hold[i] <- object.size(forest)
		}
        
        Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RerF")

############################### xgboost ##############################
num_classes <- length(unique(Y))
train <- apply(X,2,as.numeric)
label <- Y-1

		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- xgboost(data=train, label=label, objective="multi:softprob",nrounds=num_trees,max_depth=30000, num_class=num_classes, nthread=num.cores)
			ptm_hold[i] <- object.size(forest) + file.info("xgboost.model")$size
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "XGBoost")

##################################### rf ###########################
if(TRUE){

Yrf<-as.factor(as.character(Y))
		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
			forest <- randomForest(X,Yrf, ntree=num_trees)
			ptm_hold[i] <- object.size(forest)
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "RF")
}

############################ ranger ############################
X <- cbind(X,Y)
colnames(X) <- as.character(1:ncol(X))


		ptm_hold <- NA
		for (i in 1:nTimes){
			gc()
            forest <- ranger(dependent.variable.name = as.character(ncol(X)), data = X, num.trees = num_trees, num.threads = num.cores, classification=TRUE)
			ptm_hold[i] <- object.size(forest)
		}
Results <- c(Results, median(ptm_hold)/1000000/num_trees)
        Alg <- c(Alg, "Ranger")


data <- rbind(data, Results)


ress1 <- cbind(NameResults,data)
colnames(ress1) <- c("Dataset", Alg)

ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="size.Rdata")

pWidth = 300
pHeight = 300
tWidth = pWidth * .05
tHeight = .13 * pHeight

cols <- c("Ideal"="#000000", "RerF"="#009E73", "XGBoost"="#E69F00", "Ranger"="#0072B2", "RF"="#CC79A7")

png(file="exp0029.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(Dataset, value)) + geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ leg + labs(title="Forest Size", x="Dataset", y="Average Tree Size (Mb)")+ scale_fill_manual(values=cols))
dev.off()
