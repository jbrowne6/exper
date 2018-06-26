#install multiple versions of library
# change version in DESCRIPTION file then:
install.packages("rerf", lib="~/R/mult/v1")
library(rerf, lib.loc="~/R/mult/v1")
detach(package:rerf)

library(devtools)
install(args="--library=~/R/mult/v2")

library(rerf, lib.loc="~/R/mult/v2")
X <- iris[,1:4]
Y <- iris[,5]
forest <- RerF(X,Y, mat.options = list(p = ncol(X), d =ceiling(sqrt(ncol(X))), random.matrix = "rf", rho = 1/ncol(X)), seed = sample(c(1:100000),1), num.cores = 2)

detach(package:rerf)
sessionInfo() # to tell which version is loaded.
