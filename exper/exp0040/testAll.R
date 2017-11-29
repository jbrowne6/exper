library(rerf)

fImportance <- function(forest,lengthX){
      fimp <- vector("integer",lengthX)
  
  for(z in 1:length(forest$trees)){
          lengthMatAstore <- length(forest$trees[[z]]$matAstore)
      eUse <- 1:lengthMatAstore%%2 == 1
      fimp <- fimp + tabulate(forest$trees[[z]]$matAstore[eUse], lengthX)
          }
          
    fimp
}


load(file="ForJushua.RData")

X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
Y<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1
#calculate amount to bag in order to fully devolop trees
targetSpe <- .99
print("")
print("**************************************************")
print("***** Starting Multiple Runs for Accuracy ********")
print("Removing Positive Samples until .99 Sensitivity Reached")
predRate <- NA
specificity <- NA
sensitivity <- NA
trees <- 1000
fimp <- vector("integer",ncol(X))
for(i in 1:100){
                        # Grow forest       
                forest <- RerF(X,Y,min.parent =1, max.depth=0, trees=trees, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)))

fimp <- fimp + fImportance(forest, ncol(X))
}

print(fimp)
print(order(fimp))
print(sort(fimp/max(fimp)))

overall <- data.frame(Prot_Num = order(fimp), Protein = colnames(X)[order(fimp)], Importance = sort(fimp/max(fimp)))
