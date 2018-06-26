RandMat <-
    function(mat.options) {
        p <- mat.options[[1L]] # number of dimensions

        d <- mat.options[[2L]] # this determines the number of columns in the projection matrix.
        method <- mat.options[[3L]] # defines the distribution of the random projection matrix
        #Create the random matrix, a sparse matrix of 1's, -1's, and 0's.
        if (method == "binary") {
            rho <- mat.options[[4L]]
            nnzs <- round(p*d*rho)
            ind <- sort(sample.int((p*d), nnzs, replace = F))
            random.matrix <- cbind(((ind - 1L) %% p) + 1L, floor((ind - 1L) / p) + 1L,
                                   sample(c(1L, -1L), nnzs, replace = T))
        } else if (method == "rf") {
            random.matrix <- cbind(sample.int(p, d, replace = F), 1:d, rep(1L, d))
        } 
        return(random.matrix)
    }

X <- as.matrix(iris[,1:4])
Y <- as.numeric(iris[,5])
Y1 <- as.matrix(cbind(Y, apply(X, 2, order, decreasing = F)))
mat.options = list(p = ncol(X), d = ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X))
fun <- RandMat
sparseM <- fun(mat.options)
nnz <- nrow(sparseM)
nz.idx <- 1L
nClasses <- 3
#rowNums <- sample(1:150, 75)
rowNums <- sample(1:150, 150)
rowNums2 <- sort(rowNums)
NdSize <- length(rowNums)
x1 <- NA
y1 <- NA
x2 <- NA
y2 <- NA


ClassCounts1 <- tabulate(Y1[,1][rowNums], nClasses)
ClassCounts2 <- tabulate(Y[rowNums], nClasses)

while (nz.idx <= nnz) {
    # Parse sparseM to the column of the projection matrix at this iteration
    feature.idx <- sparseM[nz.idx, 2L]
    feature.nnz <- 0L
    while(sparseM[nz.idx + feature.nnz, 2L] == feature.idx) {
        feature.nnz <- feature.nnz + 1L
        if (nz.idx + feature.nnz > nnz) {
            break
        }
    }
    # lrows are the elements in sparseM that will be used to rotate the data
        lrows <- nz.idx:(nz.idx + feature.nnz - 1L)
    #x1[1L:NdSize] <- X[Y1[,sparseM[lrows,1L]+1],sparseM[lrows,1L]][Y1[,sparseM[lrows, 1L]+1][rowNums2]]
    x1[1L:NdSize] <- X[,sparseM[lrows,1L]][Y1[rowNums2,sparseM[lrows,1L]+1]]
        #y1[1L:NdSize] <- Y1[Y1[,sparseM[lrows, 1L]+1],1][Y1[rowNums2,sparseM[lrows, 1L]+1]]
    y1[1L:NdSize] <- Y1[Y1[rowNums2,sparseM[lrows, 1L]+1],1]
    #y1[1L:NdSize] <- Y1[Y1[,sparseM[lrows, 3L]+1][rowNums],1]

    Xnode[1L:NdSize] <- X[rowNums,sparseM[lrows,1L], drop=FALSE]%*%sparseM[lrows,3L, drop=FALSE]
    #Sort the projection, Xnode, and rearrange Y accordingly
    SortIdx[1:NdSize] <- order(Xnode[1L:NdSize])
    x2[1L:NdSize] <- Xnode[SortIdx[1L:NdSize]]
    y2[1L:NdSize] <- Y[rowNums][SortIdx[1:NdSize]]
}


if(mat.options[[3]] == "rf"){
    Xnode[1L:NdSize] <- X[Y[,sparseM[lrows, 1L]+1][Assigned2Node[[CurrentNode]]],sparseM[lrows,1L]]
} else{
    Xnode[1:NdSize]<-X[Assigned2Node[[CurrentNode]],sparseM[lrows,1L], drop=FALSE]%*%sparseM[lrows,3L, drop=FALSE]
}
