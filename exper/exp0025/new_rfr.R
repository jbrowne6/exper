runrfr <- function(X, Y, MinParent, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB, CNS){
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # rfr builds a randomer classification forest structure made up of a list
    # of trees.  This forest is randomer because each node is rotated before 
    # being split (as described by Tyler Tomita).  Each tree is made of nodes
    # which are numbered from left to right within a particular level (depth)
    # of a tree.  The loop over nodes when considering splits is made in the
    # same order as the numbering of the nodes. That is, nodes are traversed
    # across a single level from left to right then moves to the left-most node
    # of the next level.
    #
    #  INPUT:
    #
    # X is an n-by-p matrix, where rows represent observations and columns
    # represent features
    #
    # Y is an n-by-1 array of integer class labels. Unique class labels
    # must be contiguous and start from 1 (e.g. [0,1,2] is not okay;
    # neither is [1,3,4])
    #
    # MinParent is an integer specifying the minimum number of observations
    # a node must have in order for an attempt to split to be made.  Lower
    # values may lead to overtraining and increased training time.
    #
    # trees is the number of trees that will be in the forest.
    #
    # MaxDepth is the maximum depth that a tree can grow to.  If set to "inf"
    # then there is no maximum depth.  If set to 0 then a maximum depth is
    # calculated based on the number of classes and number of samples provided.
    #
    # bagging is the percentage of training data to withhold during each
    # training iteration.  If set to 0 then the entire training set is used
    # during every iteration.  The withheld portion of the training data
    # is used to calculate OOB error for the tree.
    #
    # ClassCt is the number of different classes in Y.  It is calculated 
    # in the calling function to prevent recalculation by each forked function 
    # when in parallel.
    #
    # FUN is the function used to create the projection matrix.  The matrix
    # returned by this function should be a p-by-u matrix where p is the
    # number of columns in the input matrix X and u is any integer > 0.
    # u can also vary from node to node.
    #
    # options is a list of inputs to the user provided projection matrix
    # creation function -- FUN.
    #
    # rotate is a boolean specifying whether or not to randomly rotate the
    # for each tree. If TRUE, then a different random rotation will be applied
    # to each bagged subsample prior to building each tree. If the number of
    # dimensions is greater than 1000, then a random subset of 1000 of the
    # dimensions will be rotated and the others will be left alone
    #
    # COOB is a boolean that determines whether or not OOB error is calculated.
    # If bagging equals zero then COOB is ignored.  If bagging does not equal 
    # zero and COOB is TRUE then OOB is calculated and printed to the screen.
    # 
    # CNS is a boolean that specifies whether to store the node size of each
    # node.
    #
    # Progress is a boolean.  When true a progress marker is printed to the 
    # screen every time a tree is grown.  This is useful for large input.
    #
    # OUTPUT:
    #
    # A forest construct made up of trees.  This forest can be used to make 
    # predictions on new inputs.  When COOB=TRUE then the output is a list
    # containing $forest and $OOBmat.  $forest is the forest structure and
    # OOBmat is the OOB error for each tree.
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Predefine variables to prevent recreation during loops
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    nClasses <- length(classCt)

    BestIdx <-0L 
    BestVar <-0L 
    BestSplitIdx<-0L 
    BestSplitValue <- 0
    w <- nrow(X)
    p <- ncol(X)
    perBag <- (1-bagging)*w

    rho<-options[[4L]]
    ps <- options[[1L]]
    ds <- options[[2L]]
    nnzs <- round(ps*ds*rho)

    if (MaxDepth == "inf"){
        Assigned2Node<- vector("list",w) 
        ClassProb <- matrix(data = 0, nrow = w, ncol = nClasses)
    }else{
        Assigned2Node <- vector("list", min(w, MaxDepth^2))
        ClassProb <- matrix(data = 0, nrow = min(w, MaxDepth^2), ncol = nClasses)
    }
    currentSize <- ceiling(log2(w))  
    treeMap <- integer(currentSize*2L)
    CutPoint <- numeric(currentSize)
    NdSize <- integer(currentSize)
    NDepth <- integer(currentSize*2L)
    ind <- NULL
    matAindex <- integer(currentSize)
    matAstore <- vector("list", currentSize)
    NDepth[1]<-1L
    CurrentNode <- 1L
    NextUnusedNode <- 2L
    NodeStack <- 1L
    highestParent <- 1L
    currIN <- 0L
    currLN <- 0L
    # Determine bagging set 
    # Assigned2Node is the set of row indices of X assigned to current node
    if(bagging != 0){
        if(replacement){
            go <- T
            while (go) {
                if(stratify){
                    ind<-sample(Cindex[[1]], classCt[1], replace=TRUE)
                    for(z in 2:nClasses){
                        ind[(classCt[z-1]+1):classCt[z]]<- sample(Cindex[[z]], classCt[z]-classCt[z-1], replace=TRUE)
                    }
                }else{
                    ind<-sample(1:w, w, replace=TRUE)
                }
                go <- all(1:w %in% ind)
            }
            Assigned2Node[[1]] <- ind
        }else{
            ind <- sample(1:w, perBag, replace = FALSE)
            Assigned2Node[[1]] <- ind        
        }
    }else{
        Assigned2Node[[1]] <- 1:w        
    }

    # main loop over nodes
    while (CurrentNode < NextUnusedNode){
        if (CurrentNode == currentSize*2L){
            currentSize <- currentSize*2L
            treeMap[currentSize*2] <- NA
            CutPoint[currentSize] <- NA
            NdSize[currentSize] <- NA
            NDepth[currentSize*2] <- NA
            matAindex[currentSize] <- NA
            matAstore[[currentSize]] <- NA
        }
        # determine working samples for current node.


        NdSize[CurrentNode] <- length(Assigned2Node[[CurrentNode]] ) #determine node size
        if(NdSize[CurrentNode]==0){stop("is ndsize 0")}
        # determine number of samples in current node then
        # determine their percentages in the node
        ClassCounts <- tabulate(Y[Assigned2Node[[CurrentNode]]] , nClasses)

        ClProb <- ClassCounts/NdSize[CurrentNode]
        # compute impurity for current node
        I <- sum(ClassCounts*(1 - ClProb))
        # if node is impure and large enough then attempt to find good split
        if (NdSize[CurrentNode] < MinParent || I <= 0 || NDepth[CurrentNode]==MaxDepth){
            treeMap[CurrentNode] <- currLN <- currLN - 1L
            ClassProb[currLN*-1L,] <- ClProb
            NodeStack <- NodeStack[-1L]
            Assigned2Node[[CurrentNode]]<-NA
            CurrentNode <- NodeStack[1L]
            if(is.na(CurrentNode)){
                break
            }
            next 
        }

        #The below returns a matrix after removing zero columns in sparseM.
        sparseM <- cbind(sample(1:ps, nnzs, TRUE), sort(sample(1:ds, nnzs, TRUE)), sample(c(1,-1), nnzs, TRUE))

# set initial values used in rotation and split finding
        nnz <- nrow(sparseM)
        MinDeltaI <- I
        nBest <- 1L
        nz.idx <- 1L
        cutVal <- NA

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
            lrows <- nz.idx:(nz.idx + feature.nnz - 1L)

            #Project input into new space
            Xnode <- X[Assigned2Node[[CurrentNode]] ,sparseM[lrows,1L], drop=FALSE]%*%sparseM[lrows,3L, drop=FALSE]

            SortIdx <- .Internal(order(TRUE, FALSE, Xnode))
            x <- Xnode[SortIdx]
            y <- Y[Assigned2Node[[CurrentNode]] ][SortIdx]

            ##################################################################
            #                    Find Best Split
            ##################################################################
            uniqueX <- !duplicated.default(x, fromLast=TRUE)
            numUnique <- sum(uniqueX)
            if (numUnique == 1){
                nz.idx <- nz.idx + feature.nnz
                next
            }
            inY <- ClassCounts != 0
                           numInY <- sum(inY)
            Ls <- vapply((1:nClasses)[inY], function(x) {cumsum(y==x)[uniqueX]}, 1:numUnique)
            Rs <- matrix(ClassCounts[inY], nrow(Ls), numInY, byrow=TRUE) - Ls

            giniSums <- .Internal(rowSums(Ls*(1-Ls/.Internal(rowSums(Ls, nrow(Rs), numInY,FALSE))) + Rs*(1-Rs/.Internal(rowSums(Rs, nrow(Rs), numInY,FALSE))), nrow(Rs), numInY, FALSE))

            DeltaI <- min(giniSums[1:(numUnique-1)])

            # Determine if this split is currently the best option
            if (DeltaI < MinDeltaI){
                minIndex <- sum(Ls[match(DeltaI, giniSums),])
                cutVal <- sum(x[minIndex:(minIndex+1)])/2
              #  if(cutVal == x[minIndex] || cutVal == x[minIndex+1]){
           # nz.idx <- nz.idx + feature.nnz
          #  next
          #      }
                MinDeltaI <- DeltaI
                nz.idxBest <- nz.idx
# It is possible that cutVal equals x[minIndex] or cutVal equals x[minIndex+1]
# This rare occurence happens when the two x values differ by an insignificant amount.
            }
            nz.idx <- nz.idx + feature.nnz
        }

        if (MinDeltaI == I) {
            treeMap[CurrentNode] <- currLN <- currLN - 1L
            ClassProb[currLN*-1L,] <- ClProb
            NodeStack <- NodeStack[-1L]
            Assigned2Node[[CurrentNode]]<-NA
            CurrentNode <- NodeStack[1L]
            if(is.na(CurrentNode)){
                break
            }
            next
        }

        # Recalculate the best projection
        feature.idx <- sparseM[nz.idxBest, 2L]
        feature.nnz <- 0L
        while(sparseM[nz.idxBest + feature.nnz, 2L] == feature.idx) {
            feature.nnz <- feature.nnz + 1L
            if (nz.idxBest + feature.nnz > nnz) {
                break
            }
        }
        lrows <- nz.idxBest:(nz.idxBest + feature.nnz - 1L)
        Xnode <-X[Assigned2Node[[CurrentNode]] ,sparseM[lrows,1L], drop=FALSE]%*%sparseM[lrows,3L, drop=FALSE]
        # find which child node each sample will go to and move
        # them accordingly
        MoveLeft <- Xnode  < cutVal

# The below check is necessary for when the cut val equals one of the 
# two values on either side of the cut val.  This rarely happens.
        if(sum(MoveLeft)== 0 || sum(MoveLeft)==length(Xnode)){

      #  if(TRUE){stop("moved 0 left or right.")}
            treeMap[CurrentNode] <- currLN <- currLN - 1L
            ClassProb[currLN*-1L,] <- ClProb
            NodeStack <- NodeStack[-1L]
            Assigned2Node[[CurrentNode]]<-NA
            CurrentNode <- NodeStack[1L]
            if(is.na(CurrentNode)){
                break
            }
            next
        }

        # Move samples left or right based on split
        Assigned2Node[[NextUnusedNode]] <- Assigned2Node[[CurrentNode]] [MoveLeft]
        Assigned2Node[[NextUnusedNode+1L]] <- Assigned2Node[[CurrentNode]] [!MoveLeft]
            Assigned2Node[[CurrentNode]]<-NA
        #remove saved indexes
        # highestParent keeps track of the highest needed matrix and cutpoint
        # this reduces what is stored in the forest structure
        if(CurrentNode>highestParent){
            highestParent <- CurrentNode
        }
        # Determine children nodes and their attributes
        treeMap[CurrentNode] <- currIN <- currIN + 1L 
        NDepth[NextUnusedNode]<-NDepth[CurrentNode]+1L
        NDepth[NextUnusedNode+1L]<-NDepth[CurrentNode]+1L
        # Pop the current node off the node stack
        # this allows for a breadth first traversal
        NodeStack <- NodeStack[-1L]
        NodeStack <- c(NextUnusedNode, NextUnusedNode+1L, NodeStack)
        NextUnusedNode <- NextUnusedNode + 2L
        # Store the projection matrix for the best split
        currMatAlength <- length(sparseM[lrows,c(1,3)])
        if (options[[3]] != "frc" && options[[3]] != "continuous") {
            matAstore[[currIN]] <-  as.integer(t(sparseM[lrows,c(1,3)]))
        } else {
            matAstore[[currIN]] <- t(sparseM[lrows,c(1,3)])
        }

        matAindex[currIN+1L] <- matAindex[currIN]+currMatAlength 
        CutPoint[currIN] <- cutVal

        CurrentNode <- NodeStack[1L]
        if(is.na(CurrentNode)){
            break
        }
    }


    # save current tree structure to the forest
    currLN <- currLN * -1L
    if (CNS) {
        if(bagging!=0 && COOB){
            return(list("treeMap"=treeMap[1:NextUnusedNode-1L], "CutPoint"=CutPoint,"ClassProb"=ClassProb[1L:currLN,,drop=FALSE], "matAstore"=unlist(matAstore), "matAindex"=matAindex[1L:(currIN+1)], "ind"=which(!(1:w %in% ind)), "NdSize" = NdSize))
        }else{
            return(list("treeMap"=treeMap[1:NextUnusedNode-1L], "CutPoint"=CutPoint,"ClassProb"=ClassProb[1L:currLN,,drop=FALSE], "matAstore"=unlist(matAstore), "matAindex"=matAindex[1L:(currIN+1)], "NdSize" = NdSize))
        }
    }else {
        if(bagging!=0 && COOB){
            return(list("treeMap"=treeMap[1:NextUnusedNode-1L], "CutPoint"=CutPoint,"ClassProb"=ClassProb[1L:currLN,,drop=FALSE], "matAstore"=unlist(matAstore), "matAindex"=matAindex[1L:(currIN+1)], "ind"=which(!(1:w %in% ind))))
        }else{
            return(list("treeMap"=treeMap[1:NextUnusedNode-1L], "CutPoint"=CutPoint,"ClassProb"=ClassProb[1L:currLN,,drop=FALSE], "matAstore"=unlist(matAstore), "matAindex"=matAindex[1L:(currIN+1)]))
        }
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Default option to make projection matrix 
#
# this is the randomer part of random forest. The sparseM 
# matrix is the projection matrix.  The creation of this
# matrix can be changed, but the nrow of sparseM should
# remain p.  The ncol of the sparseM matrix is currently
# set to mtry but this can actually be any integer > 1;
# can even greater than p.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeA <- function(options){
    p <- options[[1L]]
    d <- options[[2L]]
    method <- options[[3L]]
    if(method == 1L){
        rho<-options[[4L]]
        nnzs <- round(p*d*rho)
        sparseM <- matrix(0L, nrow=p, ncol=d)
        sparseM[sample(1L:(p*d),nnzs, replace=F)]<-sample(c(1L,-1L),nnzs,replace=T)
    }
    #The below returns a matrix after removing zero columns in sparseM.
    ind<- which(sparseM!=0,arr.ind=TRUE)
    return(cbind(ind,sparseM[ind]))        
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#   Run R-Rerf byte compiled and parallel                       
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rfr <- function(X, Y, MinParent=6L, trees=100L, MaxDepth="inf", bagging = .2, replacement=TRUE, stratify=FALSE, FUN=makeA, options=c(ncol(X), round(ncol(X)^.5),1L, 1/ncol(X)), rank.transform = FALSE, COOB=FALSE, CNS=FALSE, NumCores=0L, seed = 1L){

    #keep from making copies of X
    if(!is.matrix(X)){
        X <- as.matrix(X)
    }
    if (rank.transform) {
        X <- rank.matrix(X)
    }
    if(!is.integer(Y)){
        Y <- as.integer(Y)
    }
    uY<-length(unique(Y))
    classCt <- cumsum(tabulate(Y,uY))

    if(stratify){
        Cindex<-vector("list",uY)
        for(m in 1:uY){
            Cindex[[m]]<-which(Y==m)
        }
    }else{
        Cindex<-NULL
    }

    if (!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to create the forest.\n")
        comp_rfr <<- runrfr
    }
    if(!exists("comp_rfr")){
        setCompilerOptions("optimize"=3)
        comp_rfr <<- cmpfun(runrfr)
    }
    mcrun<- function(...) comp_rfr (X, Y, MinParent, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB=COOB, CNS=CNS)
    set.seed(seed)
    if (NumCores!=1L & require(parallel)){
        RNGkind("L'Ecuyer-CMRG")
        mc.reset.stream()
        if(NumCores==0){
            #Use all but 1 core if NumCores=0.
            NumCores=detectCores()-1L
        }
        NumCores=min(NumCores,trees)
        gc()
        forest<-mclapply(1:trees, mcrun, mc.cores =NumCores, mc.set.seed=TRUE)
    }else{
        if (NumCores != 1L){
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\n")
        }
        #Use just one core.
        forest <- lapply(1:trees, mcrun)
        #forest<- do.call(c,lapply(1:trees, mcrun))
    }

    return(forest)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       Calculate OOB Error Rate Multicore
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runerrOOBmc <- function(X, Y, Forest, z) {
    rotX<-0
    currentNode<-0L
    curr_ind <- 0
    tm <- 0L
    indexHigh <- 0L
    indexLow <- 0L

    curr_ind <- 1
    ind <- Forest[[z]]$ind
    treeMap <- Forest[[z]]$treeMap
    matAindex <- Forest[[z]]$matAindex
    matAstore <- Forest[[z]]$matAstore
    CutPoint <- Forest[[z]]$CutPoint
    ################################################Children <- Forest[[z]]$Children
    ClassProb <- Forest[[z]]$ClassProb
    numClasses <- ncol(ClassProb)
    Xt <- double(ncol(X))
    OOBmat<- matrix(0,nrow=length(ind), ncol=2+numClasses)
    for (i in ind) {
        Xt <- X[i,]
        currentNode <- 1L
        while ((tm<-treeMap[currentNode])>0L) {
            indexHigh <- matAindex[tm+1]
            indexLow <- matAindex[tm]
            s<-(indexHigh-indexLow)/2
            rotX<-sum(matAstore[(indexLow+1):indexHigh][(1:s)*2]*Xt[matAstore[(indexLow+1):indexHigh][(1:s)*2-1]])
            if (rotX<=CutPoint[tm]) {
                ############################################currentNode <- Children[tm]
                currentNode <- tm*2
            } else {
                ############################################currentNode <- Children[tm]+1
                currentNode <- tm*2+1
            }
        }
        OOBmat[curr_ind,]<- c(i,Y[i],ClassProb[tm*-1,])
        curr_ind<-curr_ind+1
    }

    return(OOBmat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       Calculate OOB Error Rate
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runerrOOB <- function(X, Y, Forest) {
    forestSize <- length(Forest) 
    OOBmat <- vector("list", forestSize)
    rotX<-double(1)
    currentNode<-integer(1)
    curr_ind <- 0
    tm <- 0L
    numClasses <- ncol(Forest[[1]]$ClassProb)
    classProb<-double(numClasses)

    Xt <- double(ncol(X))

    for (j in 1:(forestSize)) {
        ind <- Forest[[j]]$ind
        treeMap <- Forest[[j]]$treeMap
        matAindex <- Forest[[j]]$matAindex
        matAstore <- Forest[[j]]$matAstore
        CutPoint <- Forest[[j]]$CutPoint
        ##################################Children <- Forest[[j]]$Children
        ClassProb <- Forest[[j]]$ClassProb
        curr_ind <- 1
        OOBmat[[j]]<-matrix(0,nrow=length(ind), ncol=2+numClasses)
        for (i in ind) {
            currentNode <- 1L
            Xt <- X[i,]
            while ((tm<-treeMap[currentNode])>0L) {
                indexHigh <- matAindex[tm+1]
                indexLow <- matAindex[tm]
                s<-(indexHigh-indexLow)/2
                rotX<-sum(matAstore[(indexLow+1):indexHigh][(1:s)*2]*Xt[matAstore[(indexLow+1):indexHigh][(1:s)*2-1]])
                if (rotX<=CutPoint[tm]) {
                    #currentNode <- Children[tm]
                    currentNode <- tm*2
                } else {
                    #currentNode <- Children[tm]+1
                    currentNode <- tm*2+1
                }
            }
            OOBmat[[j]][curr_ind,]<- c(i,Y[i],ClassProb[tm*-1,])
            curr_ind<-curr_ind+1
        }
    }

    return(OOBmat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run OOB Error rate byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
OOBpredict <- function(X, Y, Forest, NumCores=0, rank.transform = F){
    if (rank.transform){
        X <- rank.matrix(X)
    }
    n <- nrow(X)

    if(!is.matrix(X)){
        X <- as.matrix(X)
    }
    if(!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to find the OOB error rate.\n")
        comp_errOOB <<- runerrOOB
        comp_errOOBmc <<- runerrOOBmc
    }

    if(!exists("comp_errOOB")){
        setCompilerOptions("optimize"=3)
        comp_errOOB <<- cmpfun(runerrOOB)
    } 

    if(!exists("comp_errOOBmc") && NumCores !=1){
        setCompilerOptions("optimize"=3)
        comp_errOOBmc <<- cmpfun(runerrOOBmc)
    }

    f_size <- length(Forest)
    if(NumCores!=1){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            #Start mclapply with NumCores Cores.
            comp_errOOB_caller <- function(z, ...) comp_errOOBmc(X=X,Y=Y,Forest=Forest, z)
            gc()
            OOBmat <- mclapply(1:f_size ,comp_errOOB_caller, mc.cores=NumCores)

        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\n")
            OOBmat <-comp_errOOB(X, Y, Forest)
        }
    }else{
        #Use just one core.
        OOBmat <-comp_errOOB(X, Y, Forest)
    }

    gc()
    num_classes <- ncol(Forest[[1]]$ClassProb)
    # Have to make the last entry before this bottom will work.
    OOBmat[[f_size+1]] <- matrix(0,nrow=n, ncol=2+num_classes)
    oobCounts <- vector(mode = "integer", length = n)
    for(m in 1:f_size){
        oobCounts[Forest[[m]]$ind] <- oobCounts[Forest[[m]]$ind] + 1
        for(k in 1:length(OOBmat[[m]][,1])){
            OOBmat[[f_size+1]][OOBmat[[m]][k,1],3:(2+num_classes)]<- OOBmat[[f_size+1]][OOBmat[[m]][k,1],3:(2+num_classes)] + OOBmat[[m]][k,3:(2+num_classes)]
        }
    }
    has.counts <- oobCounts != 0L
    OOBmat[[f_size+1]][has.counts, -c(1,2)] <- OOBmat[[f_size+1]][has.counts, -c(1,2)]/oobCounts[has.counts]
    numWrong<- 0L
    numTotal<- 0L
    Y<- as.numeric(Y)
    for(k in 1:n){ 
        OOBmat[[f_size+1]][k,1] <- k
        OOBmat[[f_size+1]][k,2] <- Y[k]
        if(any(OOBmat[[f_size+1]][k,3:(2+num_classes)]!=0)){
            Yhat <- which.max(OOBmat[[f_size+1]][k,3:(2+num_classes)])
            OOBmat[[f_size+1]][k, 2] <- Yhat
            if(Yhat!=Y[k]){
                numWrong <- numWrong+1L
            }
            numTotal<-numTotal+1L
        }
    }
    cat("OOB error rate: ", 100*numWrong/numTotal, "%\n")
    return(OOBmat)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          Make Predictions Multicore
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runpredictmc <- function(X, Forest, tn){

    rotX<-0
    currentNode<-0L
    curr_ind <- 0

    ind <- Forest[[tn]]$ind
    treeMap <- Forest[[tn]]$treeMap
    matAindex <- Forest[[tn]]$matAindex
    matAstore <- Forest[[tn]]$matAstore
    CutPoint <- Forest[[tn]]$CutPoint
    #########################################Children <- Forest[[tn]]$Children
    ClassProb <- Forest[[tn]]$ClassProb
    num_classes <- ncol(ClassProb)

    matA <- list(NULL)
    for(i in 1:(length(matAindex)-1)){
        matA[[i]] <- matrix(matAstore[(matAindex[i]+1):matAindex[i+1]], ncol=2, byrow=TRUE)
    }

    classProb<-double(num_classes)
    predictmat <- matrix(0,nrow=nrow(X), ncol=2+num_classes)
    Xt <- double(ncol(X))
    for (i in 1:nrow(X)){
        Xt <- X[i,]
        currentNode <- 1L
        while((tm <- treeMap[currentNode])>0L){
            rotX<-sum(matA[[tm]][,2]*Xt[matA[[tm]][,1]])
            if(rotX<=CutPoint[tm]){
                ##################################currentNode <- Children[tm]
                currentNode <- tm*2
            }else{
                ##################################currentNode <- Children[tm]+1
                currentNode <- tm*2+1
            }
        }
        classProb <- ClassProb[tm*-1,]
        guess <- which.max(classProb)
        predictmat[i,]<- c(i,guess,classProb)
    }
    return(predictmat)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          Make Predictions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runpredict <- function(X, Forest){
    forestSize <- length(Forest)
    predictmat <- vector("list", forestSize)
    rotX<-0
    currentNode<-0L
    curr_ind <- 0
    num_classes <- ncol(Forest[[1]]$ClassProb)
    classProb<-double(num_classes)
    Xt <- double(ncol(X))

    ###################################################
    #   predictmat <- matrix(0, ncol=num_classes+1, nrow=nrow(X))
    #for (i in 1:nrow(X)){
    #    classProb <- classProb*0
    #        Xt <- X[i,]
    #        for(j in 1:(forestSize)){
    #currentNode <- 1L
    #            while((tm <- Forest[[j]]$treeMap[currentNode])>0L){
    #                indexHigh <- Forest[[j]]$matAindex[tm+1]
    #                indexLow <- Forest[[j]]$matAindex[tm]
    #                s<-(indexHigh-indexLow)/2
    #                rotX<-sum(Forest[[j]]$matAstore[(indexLow+1):indexHigh][(1:s)*2]*Xt[Forest[[j]]$matAstore[(indexLow+1):indexHigh][(1:s)*2-1]])
    #                if(rotX<=Forest[[j]]$CutPoint[tm]){
    #                    currentNode <- Forest[[j]]$Children[tm]
    #                }else{
    #                    currentNode <- Forest[[j]]$Children[tm]+1
    #                }
    #            }
    #classProb <- classProb + Forest[[j]]$ClassProb[tm*-1,]
    #        }
    #predictmat[i,2:(num_classes+1)] <- classProb/forestSize
    #predictmat[i,1] <- which.max(classProb)
    #}
    #return(predictmat)
    ###################################################

    for(q in 1:forestSize){
        predictmat[[q]]<-matrix(0,nrow=nrow(X), ncol=2+num_classes)
    }

    for (i in 1:nrow(X)){
        Xt <- X[i,]
        for(j in 1:(forestSize)){
            currentNode <- 1L
            while((tm <- Forest[[j]]$treeMap[currentNode])>0L){
                indexN <- Forest[[j]]$matAindex[tm:(tm+1)]
                s<-sum(indexN[2]-indexN[1])/2
                matA <- Forest[[j]]$matAstore[(indexN[1]+1):indexN[2]]
                rotX<-sum(matA[(1:s)*2]*Xt[matA[(1:s)*2-1]])
                if(rotX<=Forest[[j]]$CutPoint[tm]){
                    #currentNode <- Forest[[j]]$Children[tm]
                    currentNode <- tm*2
                }else{
                    #currentNode <- Forest[[j]]$Children[tm]+1
                    currentNode <- tm*2+1
                }
            }
            classProb <- Forest[[j]]$ClassProb[tm*-1,]
            guess <- which.max(classProb)
            #change the way predictmat works.  Make the rows each one tree.
            predictmat[[j]][i,]<- c(i,guess,classProb)
        }
    }
    return(predictmat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run predict byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predict <- function(X, Forest, NumCores=0, rank.transform = F, Xtrain = NULL){
    if (rank.transform) {
        if (is.null(Xtrain)) {
            ############ error ############
        } else {
            X <- rank.interpolate(Xtrain, X)
        }
    }

    if(!is.matrix(X)){
        X <- as.matrix(X)
    }

    if(!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to predict.\n")
        comp_predict <<- runpredict
        comp_predictmc <<- runpredictmc
    }

    if(!exists("comp_predict")){
        setCompilerOptions("optimize"=3)
        comp_predict <<- cmpfun(runpredict)
    } 

    if(!exists("comp_predictmc") && NumCores!=1){
        setCompilerOptions("optimize"=3)
        comp_predictmc <<- cmpfun(runpredictmc)
    }

    f_size <- length(Forest)
    if(NumCores!=1){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            #Start mclapply with NumCores Cores.
            comp_predict_caller <- function(z, ...) comp_predictmc(X=X, Forest=Forest, tn = z)
            gc()
            predictmat <- mclapply(1:f_size, comp_predict_caller, mc.cores=NumCores)
        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\n")
            predictmat <-comp_predict(X, Forest)
        }
    }else{
        #Use just one core.
        predictmat <-comp_predict(X, Forest)
    }

    #return(predictmat)
    #Create summation of all predictions.
    num_classes <- ncol(Forest[[1]]$ClassProb)
    predictmat[[f_size+1]] <- matrix(0,nrow=nrow(X), ncol=2+num_classes)
    for(m in 1:f_size){
        for(k in 1:nrow(X)){
            predictmat[[f_size+1]][predictmat[[m]][k,1],3:(2+num_classes)]<- predictmat[[f_size+1]][predictmat[[m]][k,1],3:(2+num_classes)] + predictmat[[m]][k,3:(2+num_classes)]
        }
    }

    for(z in 1:nrow(X)){ 
        predictmat[[f_size+1]][z,1] <- z
        predictmat[[f_size+1]][z,2] <- which.max(predictmat[[f_size+1]][z,3:(2+num_classes)])
    }
    return(predictmat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         OOB Error as tree grows 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
errgrow <- function(Y,probmat){
    z<-integer(length(Y))
    forestSize <- length(probmat) - 1L
    numClass <- ncol(probmat[[1L]])-2
    probcurrent<- matrix(data = 0, nrow = length(Y), ncol = numClass)
    numWrong<- 0L
    numTotal<- 0L
    for(q in 1:forestSize){
        for(j in 1:nrow(probmat[[q]])){
            probcurrent[probmat[[q]][j,1L], ] <- probcurrent[probmat[[q]][j,1L], ]+probmat[[q]][j,3:(2+numClass)]
        }
        numWrong<- 0L
        numTotal<- 0L
        for(m in 1:length(Y)){
            if(any(probcurrent[m,]!=0)){
                if(which.max(probcurrent[m,])!=Y[m]){
                    numWrong <- numWrong+1L
                }
                numTotal<-numTotal+1L
            }
        }
        z[q] <- numWrong/numTotal
    }
    return(z)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run Error rate byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_rate <- function(X,Y,Forest, NumCores=0L){
    predictionMat <- predict(X, Forest, NumCores)
    return(length(which(predictionMat[[length(predictionMat)]][,2] != Y))/length(Y))
}


skip.streams <- function(n) {
    x <- .Random.seed
    for (i in seq_len(n))
        x <- nextRNGStream(x)
    assign('.Random.seed', x, pos=.GlobalEnv)
}

# function for finding the indices where a label change occurs in a sorted Y vector
groupidx <- function(Y) {
    n <- length(Y)
    class.start.idx <- vector("integer", Y[n])
    y <- 1L
    class.start.idx[1L] <- 1L
    for (i in 2:n) {
        if (Y[i] != y) {
            y <- Y[i]
            class.start.idx[y] <- i
            if (y == Y[n]) {
                break
            }
        }
    }
    return(class.start.idx)
}
