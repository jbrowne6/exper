X <- iris[,1:4]
Y <- iris[,5]

library(rerf, lib.loc="~/R/mult/v1")
forest1 <- RerF(X,Y,min.parent =1, max.depth=0, trees=10, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = 2, stratify=TRUE)
    predictions <- Predict(X, forest1, num.cores = 2)
    error.rate1 <- mean(predictions != Y)
detach(package:rerf)

library(rerf, lib.loc="~/R/mult/v2")
forest2 <- RerF(X,Y,min.parent =1, max.depth=0, trees=10, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = 2, stratify=TRUE)
    predictions <- Predict(X, forest2, num.cores = 2)
    error.rate2 <- mean(predictions != Y)
detach(package:rerf)

