runrfr <- function(X, Y, MinParent, trees, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB, Progress){
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
    # COOB is a boolean that determines whether or not OOB error is calculated.
    # If bagging equals zero then COOB is ignored.  If bagging does not equal 
    # zero and COOB is TRUE then OOB is calculated and printed to the screen.
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
    forest <- vector("list",trees)
    ClassCountsLeft <- integer(nClasses)
    ClassCountsRight <- integer(nClasses)
    ClassProbLeft <- double(nClasses)
    ClassProbRight <- double(nClasses)
    consecutive <- 0L
    BV <- NA # vector in case of ties
    BS <- NA # vector in case of ties
    MaxDeltaI <- 0
    nBest <- 1L
    BestIdx <-0L 
    BestVar <-0L 
    BestSplitIdx<-0L 
    BestSplitValue <- 0
    w <- nrow(X)
    p <- ncol(X)
    perBag <- (1-bagging)*w
    Xnode<-double(w) # allocate space to store the current projection
    SortIdx<-integer(w) 
    y <- integer(w)
    if(object.size(X) > 1000000){
        OS<-TRUE
    }else{
        OS<-FALSE
    }

    # Calculate the Max Depth and the max number of possible nodes
    if(MaxDepth == "inf"){
        StopNode <- 2L*w #worst case scenario is 2*(w/(minparent/2))-1
        MaxNumNodes <- 2L*w # number of tree nodes for space reservation
    }else{
        if(MaxDepth==0){
            MaxDepth <- ceiling((log2(w)+log2(nClasses))/2)
        }
        StopNode <- 2L^(MaxDepth)
        MaxNumNodes <- 2L^(MaxDepth+1L)  # number of tree nodes for space reservation
    }

    ClassProb <- matrix(data = 0, nrow = MaxNumNodes, ncol = nClasses, byrow=TRUE)
    CutPoint <- double(MaxNumNodes)
    Children <- matrix(data = 0L, nrow = MaxNumNodes,ncol = 2L, byrow=TRUE)
    NDepth <- integer(MaxNumNodes)
    matA <- vector("list", MaxNumNodes) 
    Assigned2Node<- vector("list",MaxNumNodes) 
    ind <- double(w)
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #                            Start tree creation
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for(treeX in 1:trees){
        # intialize values for new tree before processing nodes
        ClassProb[] <- 0
        CutPoint[] <- 0
        Children[] <- 0L
        NDepth[]<- 0L #delete this?
        NDepth[1]<-1L
        CurrentNode <- 1L
        NextUnusedNode <- 2L
        NodeStack <- 1L
        highestParent <- 1L
        ind[] <- 0L
        # Determine bagging set 
        # Assigned2Node is the set of row indices of X assigned to current node
        if(bagging != 0){
            if(replacement){
                if(stratify){
                    ind[1:classCt[1]]<-sample(Cindex[[1]], classCt[1], replace=TRUE)
                    for(z in 2:nClasses){
                        ind[(classCt[z-1]+1):classCt[z]]<- sample(Cindex[[z]], classCt[z]-classCt[z-1], replace=TRUE)
                    }
                }else{
                    ind<-sample(1:w, w, replace=TRUE)
                }
                Assigned2Node[[1]] <- ind
            }else{
                ind[1:perBag] <- sample(1:w, perBag, replace = FALSE)
                Assigned2Node[[1]] <- ind[1:perBag]        
            }
        }else{
            Assigned2Node[[1]] <- 1:w        
        }

        # main loop over nodes
        while (CurrentNode < NextUnusedNode && CurrentNode < StopNode){
            # determine working samples for current node.
            NodeRows <- Assigned2Node[CurrentNode] 
            Assigned2Node[[CurrentNode]]<-NA #remove saved indexes
            NdSize <- length(NodeRows[[1L]]) #determine node size
            # determine number of samples in current node then
            # determine their percentages in the node
            ClassCounts <- tabulate(Y[NodeRows[[1L]]], nClasses)
            ClProb <- ClassCounts/NdSize
            # compute impurity for current node
            I <- sum(ClassCounts*(1 - ClProb))
            # if node is impure and large enough then attempt to find good split
            if (NdSize < MinParent || I <= 0 || NDepth[CurrentNode]==MaxDepth || NextUnusedNode+1L >= StopNode){
                ClassProb[CurrentNode,] <- ClProb
                NodeStack <- NodeStack[-1L]
                CurrentNode <- NodeStack[1L]
                if(is.na(CurrentNode)){
                    break
                }
                next 
            }

            # create projection matrix (sparseM) by calling the custom function FUN
            sparseM <- FUN(options)
            # Check each projection to determine which splits the best.
            MaxDeltaI <- 0
            nBest <- 1L
            for(q in unique(sparseM[,2])){
                #Project input into new space
                lrows <- which(sparseM[,2]==q)
                Xnode[1:NdSize] <- X[NodeRows[[1L]],sparseM[lrows,1], drop=FALSE]%*%sparseM[lrows,3, drop=FALSE]
                #Sort the projection, Xnode, and rearrange Y accordingly
                SortIdx[1:NdSize] <- order(Xnode[1:NdSize] )
                y[1:NdSize] <- Y[NodeRows[[1L]]][SortIdx[1:NdSize]]

                ##################################################################
                #                    Find Best Split
                ##################################################################

                # initialize variables for loop through projection
                consecutive <- 0L
                ClassCountsLeft[] <- 0L
                ClassCountsRight[] <- ClassCounts
                yl <- y[1L]
                for (m in 1:(NdSize-1L)){
                    yr <- y[m+1L]
                    consecutive <- consecutive +1L
                    # only calculate Class Probs if current class is different
                    # from the previous class
                    if (yl == yr ){
                        next
                    }
                    ClassCountsLeft[yl] <-ClassCountsLeft[yl] + consecutive 
                    ClassCountsRight[yl] <- ClassCountsRight[yl] - consecutive
                    consecutive <- 0L
                    yl <- yr

                    # Calculate class probabilities
                    ClassProbLeft <- ClassCountsLeft/m
                    ClassProbRight <- ClassCountsRight/(NdSize-m)
                    # Calculate change in impurity based on current split
                    DeltaI <- I - sum(ClassCountsLeft*(1 - ClassProbLeft) + ClassCountsRight*(1 - ClassProbRight))

                    # Determine if this split is currently the best option
                    if (DeltaI >= MaxDeltaI){
                        # Save current best DeltaI
                        if (DeltaI != MaxDeltaI){
                            MaxDeltaI <- DeltaI
                            nBest <- 1L
                            BV[nBest] <- q
                            BS[nBest] <- m
                        }else{
                            # Save all DeltaI equal to current max DeltaI
                            nBest <- nBest + 1L
                            BV[nBest] <- q
                            BS[nBest] <- m
                        }
                    }
                }
            }#end loop through projections.

            # If there were multiple best splits then randomly choose
            # between the best.
            if (nBest > 1L){
                # Break ties at random
                BestIdx <- ceiling(runif(1,0,nBest))
                BestVar <- BV[BestIdx]
                BestSplitIdx <- BS[BestIdx]
            }else{
                BestVar <- BV[1L]
                BestSplitIdx <- BS[1L]
            }
            # Recalculate the best projection
            lrows<-which(sparseM[,2L]==BestVar)
            Xnode[1:NdSize]<-X[NodeRows[[1L]],sparseM[lrows,1], drop=FALSE]%*%sparseM[lrows,3, drop=FALSE]

            # reorder the projection and find the cut value
            SortIdx[1:NdSize] <- order(Xnode[1:NdSize] )
            # determine split value as mean of values on either side of split
            BestSplitValue <- sum(Xnode[SortIdx[BestSplitIdx:(BestSplitIdx+1L)]])/2

            # find which child node each sample will go to and move
            # them accordingly
            MoveLeft <- Xnode[1:NdSize]  <= BestSplitValue
            numMove <- sum(MoveLeft)
            #Check to see if a split occured, or if all elements being moved one direction.
            if(numMove!=0L && numMove!=NdSize){
                # Move samples left or right based on split
                Assigned2Node[[NextUnusedNode]] <- NodeRows[[1L]][MoveLeft]
                Assigned2Node[[NextUnusedNode+1L]] <- NodeRows[[1L]][!MoveLeft]

                #highest Parent keeps track of the highest needed matrix and cutpoint
                # this reduces what is stored in the forest structure
                if(CurrentNode>highestParent){
                    highestParent <- CurrentNode
                }
                # Determine children nodes and their attributes
                Children[CurrentNode,1L] <- NextUnusedNode
                Children[CurrentNode,2L] <- NextUnusedNode+1L
                NDepth[NextUnusedNode]=NDepth[CurrentNode]+1L
                NDepth[NextUnusedNode+1L]=NDepth[CurrentNode]+1L
                # Pop the current node off the node stack
                # this allows for a breadth first traversal
                NodeStack <- NodeStack[-1L]
                NodeStack <- c(NextUnusedNode, NextUnusedNode+1L, NodeStack)
                NextUnusedNode <- NextUnusedNode + 2L
                # Store the projection matrix for the best split
                matA[[CurrentNode]] <- as.integer(t(sparseM[which(sparseM[,2]==BestVar),c(1,3)]))
                CutPoint[CurrentNode] <- BestSplitValue
            }else{
                # There wasn't a good split so ignore this node and move to the next
                NodeStack <- NodeStack[-1L]
            }
            # Store ClassProbs for this node.
            # Only really useful for leaf nodes, but could be used instead of recalculating 
            # at each node which is how it is currently.
            ClassProb[CurrentNode,] <- ClProb
            CurrentNode <- NodeStack[1L]
            if(is.na(CurrentNode)){
                break
            }
        }
        #If input is large then garbage collect prior to adding onto the forest structure.
        if(OS){
            gc()
        }
        # save current tree structure to the forest
        if(bagging!=0 && COOB){
            forest[[treeX]] <- list("CutPoint"=CutPoint[1:highestParent],"ClassProb"=ClassProb[1L:(NextUnusedNode-1L),,drop=FALSE],"Children"=Children[1L:(NextUnusedNode-1L),,drop=FALSE], "matA"=matA[1L:highestParent], "ind"=which(!(1:w %in% ind)))
        }else{
            forest[[treeX]] <- list("CutPoint"=CutPoint[1:highestParent],"ClassProb"=ClassProb[1L:(NextUnusedNode-1L),,drop=FALSE],"Children"=Children[1L:(NextUnusedNode-1L),,drop=FALSE], "matA"=matA[1L:highestParent])
        }
        if(Progress){
            cat("|")
        }
    }
    return(forest)
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
#                       Calculate Error Rate
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
RunErr <- function(X,Y,Forest, index=0L, chunk_size=0L){
    if(index && chunk_size){
        X<- X[(((index-1)*chunk_size)+1L):(index*chunk_size),,drop=FALSE]
    }
    n <- nrow(X)
    forestSize <- length(Forest)
    classProb<-double(length(Forest[[1]]$ClassProb[1,]))
    z <- integer()
    rotX<-0
    currentNode<-0L
    for(i in 1:n){
        classProb[] <- 0
        for(j in 1:forestSize){
            Tree <- Forest[[j]]
            currentNode <- 1L
            while(Tree$Children[currentNode]!=0L){
                s<-length(Tree$matA[[currentNode]])/2
                rotX<-sum(Tree$matA[[currentNode]][(1:s)*2]*X[i,Tree$matA[[currentNode]][(1:s)*2-1]])
                if(rotX<=Tree$CutPoint[currentNode]){
                    currentNode <- Tree$Children[currentNode,1L]
                }else{
                    currentNode <- Tree$Children[currentNode,2L]
                }
            }
            classProb <- classProb + Tree$ClassProb[currentNode,]
        }
        z <- c(z,order(classProb,decreasing=T)[1L])
    }
    if(!index || !chunk_size){
        return(sum(z!=Y))
    }else{
        return(sum(z!=Y[(((index-1)*chunk_size)+1L):(index*chunk_size)]))
    }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       Calculate OOB Error Rate
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runerrOOB <- function(X,Y,Forest, index=0L, chunk_size=0L){
    if(index && chunk_size){
        Forest<- Forest[(((index-1)*chunk_size)+1L):(index*chunk_size)]
    }
    forestSize <- length(Forest) 
    OOBmat <- vector("list", forestSize)
    rotX<-0
    currentNode<-0L
    curr_ind <- 0
    classProb<-double(length(Forest[[1]]$ClassProb[1,]))
    for(j in 1:(forestSize)){
        OOBmat[[j]]<-matrix(0,nrow=length(Forest[[j]]$ind), ncol=2+ncol(Forest[[1]]$ClassProb))
        curr_ind <- 1
        for (i in Forest[[j]]$ind){
            currentNode <- 1L
            while(Forest[[j]]$Children[currentNode]!=0L){
                s<-length(Forest[[j]]$matA[[currentNode]])/2
                rotX <-sum(Forest[[j]]$matA[[currentNode]][(1:s)*2]*X[i,Forest[[j]]$matA[[currentNode]][(1:s)*2-1]])
                if(rotX<=Forest[[j]]$CutPoint[currentNode]){
                    currentNode <- Forest[[j]]$Children[currentNode,1L]
                }else{
                    currentNode <- Forest[[j]]$Children[currentNode,2L]
                }
            }
            classProb <- Forest[[j]]$ClassProb[currentNode,]
            OOBmat[[j]][curr_ind,]<- c(i,Y[i],classProb)
            curr_ind<-curr_ind+1
        }
    }
    return(OOBmat)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          Make Predictions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runpredict <- function(X,Forest, index=0L, chunk_size=0L){
    if(index && chunk_size){
        Forest<- Forest[(((index-1)*chunk_size)+1L):(index*chunk_size)]
    }

    forestSize <- length(Forest)
    predictmat <- vector("list", forestSize)
    rotX<-0
    currentNode<-0L
    curr_ind <- 0
    classProb<-double(length(Forest[[1]]$ClassProb[1,]))
    num_classes <- ncol(Forest[[1]]$ClassProb)
    for(j in 1:(forestSize)){
        predictmat[[j]]<-matrix(0,nrow=nrow(X), ncol=2+num_classes)
        for (i in 1:nrow(X)){
            currentNode <- 1L
            while(Forest[[j]]$Children[currentNode]!=0L){
                s<-length(Forest[[j]]$matA[[currentNode]])/2
                rotX <-sum(Forest[[j]]$matA[[currentNode]][(1:s)*2]*X[i,Forest[[j]]$matA[[currentNode]][(1:s)*2-1]])
                if(rotX<=Forest[[j]]$CutPoint[currentNode]){
                    currentNode <- Forest[[j]]$Children[currentNode,1L]
                }else{
                    currentNode <- Forest[[j]]$Children[currentNode,2L]
                }
            }
            classProb <- Forest[[j]]$ClassProb[currentNode,]
            guess <- order(classProb,decreasing=T)[2L]
            predictmat[[j]][i,]<- c(i,guess,classProb)
        }
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
                if(order(probcurrent[m,],decreasing=T)[1L]!=Y[m]){
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
#   Run R-Rerf byte compiled and parallel                       
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rfr <- function(X, Y, MinParent=6L, trees=100L, MaxDepth=0L, bagging = .2, replacement=TRUE, stratify=FALSE, FUN=makeA, options=c(ncol(X), round(ncol(X)^.5),1L, 1/ncol(X)), COOB=FALSE, Progress=FALSE, NumCores=0L){

    #keep from making copies of X
    X <- as.matrix(X, byrow=TRUE)
    if(!is.integer(Y)){
        Y <- as.integer(Y)
    }
    uY<-length(unique(Y))
    classCt <- tabulate(Y,uY)
    for(q in 2:uY){
        classCt[q] <- classCt[q]+classCt[q-1]
    }
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

    if (NumCores!=1L){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            if (trees%%NumCores==0){
                tree_per <- trees/NumCores
                mcrun<- function(...) comp_rfr (X, Y, MinParent, trees=tree_per, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB=COOB, Progress=Progress)
                forest<-do.call(c,mclapply(seq_len(NumCores), mcrun, mc.cores =NumCores, mc.set.seed=TRUE))
            }else{
                if(trees > NumCores){
                    tree_per <- floor(trees/NumCores)
                    mcrun<- function(...) comp_rfr (X, Y, MinParent, trees=tree_per, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB=COOB, Progress=Progress)
                    forest<- do.call(c, mclapply(seq_len(NumCores), mcrun, mc.cores=NumCores))
                }
                mcrun<- function(...) comp_rfr (X, Y, MinParent, trees=1, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB=COOB, Progress=Progress)
                forest<-c(forest,do.call(c,mclapply(seq_len(trees%%NumCores), mcrun, mc.cores=trees%%NumCores, mc.set.seed=TRUE)))
            }
        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\nThis will increase the time required to create the forest\n")
            forest<-comp_rfr(X, Y, MinParent, trees, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB=COOB, Progress=Progress)
        }
    }else{
        #Use just one core.
        forest<-comp_rfr(X, Y, MinParent, trees, MaxDepth, bagging, replacement, stratify, Cindex, classCt, FUN, options, COOB, Progress=Progress)
    }
    if(Progress){
        cat("\n\n")
    }

    return(forest)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run OOB Error rate byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
OOBpredict <- function(X, Y, Forest, NumCores=0){
    if(!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to find the OOB error rate.\n")
        comp_errOOB <<- runerrOOB
    }

    if(!exists("comp_errOOB")){
        setCompilerOptions("optimize"=3)
        comp_errOOB <<- runerrOOB
        #comp_errOOB <<- cmpfun(runerrOOB)
    } 

    f_size <- length(Forest)
    X<- as.matrix(X, byrow=TRUE)
    if(NumCores!=1){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            #Start mclapply with NumCores Cores.
            if (f_size%%NumCores==0){
                chunk_size <- f_size/NumCores
                comp_errOOB_caller <- function(z, ...) comp_errOOB(X=X,Y=Y,Forest=Forest,index=z, chunk_size=chunk_size)
                OOBmat <- do.call(c,mclapply(1:NumCores,comp_errOOB_caller, mc.cores=NumCores))
            }else{
                if(f_size > NumCores){
                    chunk_size <- floor(f_size/NumCores)
                    comp_errOOB_caller <- function(z, ...) comp_errOOB(X=X,Y=Y,Forest=Forest,index=z, chunk_size=chunk_size)
                    OOBmat <- do.call(c,mclapply(1:NumCores,comp_errOOB_caller, mc.cores=NumCores))
                }
                comp_errOOB_caller <- function(z, ...) comp_errOOB(X=X,Y=Y,Forest=Forest[(f_size*chunk_size+1):f_size],index=z, chunk_size=1L)
                OOBmat <- c(OOBmat,do.call(c, (mclapply(1:(f_size%%NumCores), comp_errOOB_caller, mc.cores=(f_size%%NumCores)))))
            }
        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\nThis will increase the time required to create the forest\n")
            OOBmat <-comp_errOOB(X, Y, Forest)
        }
    }else{
        #Use just one core.
        OOBmat <-comp_errOOB(X, Y, Forest)
    }

    num_classes <- ncol(Forest[[1]]$ClassProb)
    # Have to make the last entry before this bottom will work.
    OOBmat[[f_size+1]] <- matrix(0,nrow=nrow(X), ncol=2+num_classes)
    #OOBmat[[forestSize]][i,] <- OOBmat[[forestSize]][i,] + classProb
    for(m in 1:f_size){
        for(k in 1:length(OOBmat[[m]][,1])){
            OOBmat[[f_size+1]][OOBmat[[m]][k,1],3:(2+num_classes)]<- OOBmat[[f_size+1]][OOBmat[[m]][k,1],3:(2+num_classes)] + OOBmat[[m]][k,3:(2+num_classes)]
        }
    }
    numWrong<- 0L
    numTotal<- 0L
    Y<- as.numeric(Y)
    for(k in 1:nrow(X)){ 
        OOBmat[[f_size+1]][k,1] <- k
        OOBmat[[f_size+1]][k,2] <- Y[k]
        if(any(OOBmat[[f_size+1]][k,3:(2+num_classes)]!=0)){
            if(order(OOBmat[[f_size+1]][k,3:(2+num_classes)],decreasing=T)[1L]!=Y[k]){
                numWrong <- numWrong+1L
            }
            numTotal<-numTotal+1L
        }
    }
    cat("OOB error rate: ", 100*numWrong/numTotal, "%\n")
    return(OOBmat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run predict byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predict <- function(X, Forest, NumCores=0){
    if(!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to predict.\n")
        comp_predict <<- runpredict
    }

    if(!exists("comp_predict")){
        setCompilerOptions("optimize"=3)
        comp_predict <<- runpredict
    } 

    f_size <- length(Forest)
    X<- as.matrix(X, byrow=TRUE)
    if(NumCores!=1){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            #Start mclapply with NumCores Cores.
            if (f_size%%NumCores==0){
                chunk_size <- f_size/NumCores
                comp_predict_caller <- function(z, ...) comp_predict(X=X, Forest=Forest,index=z, chunk_size=chunk_size)
                predictmat <- do.call(c,mclapply(1:NumCores,comp_predict_caller, mc.cores=NumCores))
            }else{
                if(f_size > NumCores){
                    chunk_size <- floor(f_size/NumCores)
                    comp_predict_caller <- function(z, ...) comp_predict(X=X,Forest=Forest,index=z, chunk_size=chunk_size)
                    predictmat <- do.call(c,mclapply(1:NumCores,comp_predict_caller, mc.cores=NumCores))
                }
                comp_predict_caller <- function(z, ...) comp_predict(X=X,Forest=Forest[(f_size*chunk_size+1):f_size],index=z, chunk_size=1L)
                predictmat <- c(predictmat,do.call(c, (mclapply(1:(f_size%%NumCores), comp_predict_caller, mc.cores=(f_size%%NumCores)))))
            }
        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\nThis will increase the time required to create the forest\n")
            predictmat <-comp_predict(X, Forest)
        }
    }else{
        #Use just one core.
        predictmat <-comp_predict(X, Forest)
    }

    num_classes <- ncol(Forest[[1]]$ClassProb)
    predictmat[[f_size+1]] <- matrix(0,nrow=nrow(X), ncol=2+num_classes)
    for(m in 1:f_size){
        for(k in 1:nrow(X)){
            predictmat[[f_size+1]][predictmat[[m]][k,1],3:(2+num_classes)]<- predictmat[[f_size+1]][predictmat[[m]][k,1],3:(2+num_classes)] + predictmat[[m]][k,3:(2+num_classes)]
        }
    }

for(z in 1:nrow(X)){ 
        predictmat[[f_size+1]][z,1] <- z
        predictmat[[f_size+1]][z,2] <- order(predictmat[[f_size+1]][z,3:(2+num_classes)],decreasing=T)[1L]
    }
    return(predictmat)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      Run Error rate byte compiled and parallel 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_rate <- function(X,Y,Forest, NumCores=0L){
    if(!is.null(Forest$forest)){
        Forest<-Forest$forest
    }
    if(!require(compiler)){
        cat("You do not have the 'compiler' package.\nExecution will continue without compilation.\nThis will increase the time required to find the error rate.\n")
        comp_err <<- RunErr
    }

    if(!exists("comp_err")){
        setCompilerOptions("optimize"=3)
        comp_err <<- cmpfun(RunErr)
    } 

    X<- as.matrix(X, byrow=TRUE)
    if(NumCores!=1){
        if(require(parallel)){
            if(NumCores==0){
                #Use all but 1 core if NumCores=0.
                NumCores=detectCores()-1L
            }
            #Start mclapply with NumCores Cores.
            if (nrow(X)%%NumCores==0){
                chunk_size <- nrow(X)/NumCores
                comp_err_caller <- function(z, ...) comp_err(X=X,Y=Y,Forest=Forest,index=z, chunk_size=chunk_size)
                total_misclassified <- sum(as.numeric(mclapply(1:NumCores,comp_err_caller, mc.cores=NumCores)))
            }else{
                if(nrow(X) > NumCores){
                    chunk_size <- floor(nrow(X)/NumCores)
                    comp_err_caller <- function(z, ...) comp_err(X=X,Y=Y,Forest=Forest,index=z, chunk_size=chunk_size)
                    total_misclassified <- sum(as.numeric(mclapply(1:NumCores,comp_err_caller, mc.cores=NumCores)))
                }
                comp_err_caller <- function(z, ...) comp_err(X=X[(NumCores*chunk_size+1L):nrow(X),,drop=FALSE],Y=Y[(NumCores*chunk_size+1L):nrow(X)],Forest=Forest,index=z, chunk_size=1L)
                total_misclassified <- total_misclassified + sum(as.numeric(mclapply(1:(nrow(X)%%NumCores), comp_err_caller, mc.cores=(nrow(X)%%NumCores))))
            }
        }else{
            #Parallel package not available.
            cat("Package 'parallel' not available.\nExecution will continue without parallelization.\nThis will increase the time required to create the forest\n")
            total_misclassified <-comp_err(X, Y, Forest)
        }
    }else{
        #Use just one core.
        total_misclassified <-comp_err(X, Y, Forest)
    }
    return(total_misclassified/nrow(X))
}
