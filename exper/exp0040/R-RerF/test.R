library(rerf)
X<- matrix(runif(30*5000)+.10, ncol=30)
X<- rbind(X,matrix(runif(30*5000), ncol=30))
Y<-as.numeric(c(rep(2,5000),rep(1,5000)))

forest <- RerF(X,Y,min.parent =1,bagging=0, max.depth=0, trees=1, store.ns=TRUE, mat.options = list(p = ncol(X), d = ncol(X), random.matrix = "rf", rho = 1/ncol(X)))
forest$trees

X1<- matrix(runif(30*5000)+.10, ncol=30)
X1 <- rbind(X1,matrix(runif(30*5000), ncol=30))
Y1<-as.numeric(c(rep(2,5000),rep(1,5000)))

depth <- PredictD(X1,forest)
df <- data.frame(x=depth)

ggplot(df,aes(x)) + stat_ecdf(geom = "step", pad = FALSE)




load(file="ForJushua.RData")
X<- as.matrix(ForJoshua1[,2:length(ForJoshua1[1,])])
Y<-as.numeric(ForJoshua1[,1]+1)


forest <- RerF(X,Y,min.parent =1,bagging=0, max.depth=0, trees=1, store.ns=TRUE, mat.options = list(p = ncol(X), d = ncol(X), random.matrix = "rf", rho = 1/ncol(X)))
forest$trees

X1<- matrix(runif(30*5000)+.10, ncol=30)
X1 <- rbind(X1,matrix(runif(30*5000), ncol=30))
Y1<-as.numeric(c(rep(2,5000),rep(1,5000)))

depth <- PredictD(X1,forest)
df <- data.frame(x=depth)

ggplot(df,aes(x)) + stat_ecdf(geom = "step", pad = FALSE)


X<- as.matrix(ForJoshua2[,c(1:(length(ForJoshua2[1,])-4), (length(ForJoshua2[1,])-2):length(ForJoshua2[1,]))])
Y<-as.numeric(ForJoshua2[,(length(ForJoshua2[1,])-3)])+1

X<- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])
Y<-as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1

forest <- RerF(X,Y)
error.rate <- Predict
