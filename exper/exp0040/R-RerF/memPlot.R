library(rerf)
library(ggplot2)
set.seed(44)
X <- iris[,1:4]
Y <- iris[,5]
num.trees <- 10
forest <- RerF(X, Y, trees <- num.trees)
tree.num <- NA
node.num <- NA
plot.num <- NA
curr.mem <- 1


test.sample <- sample(1:nrow(X),1)

for(z in 1:num.trees){
  m <- 1
  while ((tm <- forest$trees[[z]]$treeMap[m]) > 0L) {
    tree.num[curr.mem] <- z
    plot.num[curr.mem] <- z
    node.num[curr.mem] <- m
    curr.mem <- curr.mem+1
    indexHigh <- forest$trees[[z]]$matAindex[tm+1L]
    indexLow <- forest$trees[[z]]$matAindex[tm] + 1L
    s <- (indexHigh - indexLow + 1L)/2L
    curr.val <- as.numeric(X[test.sample, forest$trees[[z]]$matAstore[indexLow:indexHigh][(1:s)*2L-1L], drop = F])%*%
      as.matrix(forest$trees[[z]]$matAstore[indexLow:indexHigh][(1:s)*2L], drop=FALSE)
    if (curr.val <= forest$trees[[z]]$CutPoint[tm]){
      m <- tm * 2
    }else{
      m <- tm*2+1
    }
  }
  tree.num[curr.mem] <- z
  node.num[curr.mem] <- m
  plot.num[curr.mem] <- z
}
  
memPlot <- data.frame(tree.numc <- tree.num, node.numc <- node.num, treec <- as.factor(plot.num))
print(ggplot(memPlot, aes(x = node.numc, y = tree.numc, colour = treec)) + geom_line() + geom_point())

#memPlot2 <- memPlot
node.num[39] <- 8
node.num[38] <- 7

node.num[33] <-2

node.num[28] <-2

node.num[25] <-7

node.num[18] <- 5
node.num[14] <- 10
memPlot2 <- data.frame(tree.numc <- tree.num, node.numc <- node.num, treec <- as.factor(plot.num))
print(ggplot(memPlot2, aes(x = node.numc, y = tree.numc, colour = treec)) + geom_line() + geom_point())
