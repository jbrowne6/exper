library(rerf)
set.seed(42)
X <- iris[,1:4]
Y <- iris[,5]
num.trees <- 10
forest <- RerF(X, Y, trees <- num.trees)
tree.num <- NA
node.num <- NA
curr.mem <- 1


test.sample <- sample(1:nrow(X),1)

for(z in 1:num.trees){
  m <- 1
  while ((tm <- forest$trees[[z]]$treeMap[m]) > 0L) {
    tree.num[curr.mem] <- z
    node.num[curr.mem] <- m
    curr.mem <- curr.mem+1
    indexHigh <- forest$trees[[z]]$matAindex[tm+1L]
    indexLow <- forest$trees[[z]]$matAindex[tm] + 1L
    s <- (indexHigh - indexLow + 1L)/2L
    curr.val <- as.numeric(X[test.sample, forest$trees[[z]]$matAstore[indexLow:indexHigh][(1:s)*2L-1L], drop = F])%*%
      as.matrix(forest$trees[[z]]$matAstore[indexLow:indexHigh][(1:s)*2L], drop=FALSE)
    if (curr.val <= forest$trees[[z]]$CutPoint[tm]){
      m <- m * 2
    }else{
      m <- m*2+1
    }
  }
  tree.num[curr.mem] <- z
  node.num[curr.mem] <- m
}
  
memPlot <- data.frame(tree.num <- 1:num.trees)
