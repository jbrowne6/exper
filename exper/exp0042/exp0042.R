# ---
# title: "Accuracy on subsample of MNIST as feature number grows"
# author: "James Browne"
# date: "May 16 2017"
#output: html_document
# ---

library(ggplot2)
library(rerf)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

runRerF <- TRUE
nTimes <- 10
num_trees <-  500
median_time <- NA
num.threads <- 10#50# 25
samp_size <-c(.05, .10, .20, .40, .80) 
data_percent <- NULL
pFac <- NULL
NameResults <- NULL
Results <- NULL
data <- data.frame()
sink("out.log")
zp <- .05
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
    for(qp in c(1,2,4,8,16)){
    print(paste("starting MNIST RERF: ", qp,"\n"))
    if(runRerF){
        ptm_hold <- NA
        for (i in 1:nTimes){
            X_choose <- NULL
    for(zq in unique(Y)){
        X1 <- sample(1:length(Y[Y==zq]), zp*length(Y[Y==zq]), replace=FALSE)
        X_choose <- c(X_choose, X1)
    }
    Xu <- X[X_choose,]
    Yu <- Y[X_choose]
            gc()

            forest <- RerF(Xu,
                           Yu, 
                           trees=num_trees, 
                           bagging=.3, 
                           min.parent=1, 
                           max.depth=0, 
                           store.oob=TRUE, 
                           stratify=TRUE, 
                           num.cores=num.threads, 
                           seed=sample(1:100000,1),
                           mat.options = list(p = ncol(Xu), d = qp*ceiling(sqrt(ncol(Xu))), 
                                              random.matrix = "binary", rho = qp/ncol(Xu)))
            predictions <- Predict(Xt, forest, num.cores = num.threads)
            error.rate <- mean(predictions != Yt)
            ptm_hold[i] <- error.rate
        }
        Results <- c(Results, ptm_hold)
        pFac <- c(pFac, rep(qp, nTimes))
        data_percent <- c(data_percent, rep(zp, nTimes))
        NameResults <- c(NameResults, rep("MNIST",nTimes))
    }
}
sink()
##############################################################
#################   Print ###################################

ress1<-data.frame(pFac=as.factor(pFac), Results = Results)

#ress1 <- melt(ress1, id.vars='Dataset')
save(ress1, file="accuracy.Rdata")
#save(data_percent, file="dp.Rdata")

pWidth = 300
pHeight = 300
tWidth = pWidth * .05
tHeight = .13 * pHeight

png(file="exp0042.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(x=pFac, y=Results)) + geom_point() +leg + labs(title="Error Rates vs P Factor", x="P Factor", y="Error Rate "))
dev.off()
