#---
# title: "Test speed-up and scale-up of R-Rerf against xg and ranger"
# author: "James Browne"
# date: "Oct 10 2017"
# output: html_document
# ---
library(ggplot2)

data_summary <- function(data, varname, groupnames){
      require(plyr)
  summary_func <- function(x, col){
          c(mean = mean(x[[col]], na.rm=TRUE),
                  sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                                      varname)
    data_sum <- rename(data_sum, c("mean" = varname))
     return(data_sum)
}

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))


load(file="results.Rdata")

data.test$dataset <- as.factor(data.test$dataset)
data.test$depth <- as.numeric(data.test$depth)
data.test$processTime <- as.numeric(data.test$processTime)
data.test$accuracy <- as.numeric(data.test$accuracy)

data.testAccuracy <- data_summary(data.test, varname="accuracy", groupnames=c("dataset", "depth"))

cols <- c("higgs"="#000000", "mnist"="#009E73", "cifar10"="#E69F00")

g <- ggplot(data=data.testAccuracy, aes(x=depth, y=accuracy,color = dataset))
g <- g + geom_line(size=1.5)
g <- g + leg + labs(title="Accuracy vs Depth", x="Training Depth", y="Error Rate ", subtitle="500 trees, 1-40 depth")
g <- g + scale_color_manual(values=cols)


png(file="exp0047Acc.png")
print(g)
dev.off()

data.testTime <- data_summary(data.test,varname="processTime", groupnames=c("dataset", "depth"))

cols <- c("higgs"="#000000", "mnist"="#009E73", "cifar10"="#E69F00")

g <- ggplot(data=data.testTime, aes(x=depth, y=processTime,color = dataset))
g <- g + geom_line(size=1.5)
g <- g + leg + labs(title="Training Time vs Depth", x="Training Depth", y="Training Time (s)", subtitle="500 trees, 1-40 depth")
g <- g + scale_color_manual(values=cols)

png(file="exp0047Time.png")
print(g)
dev.off()


accuracy <- data.testAccuracy$accuracy
data.testBoth <- cbind(data.testTime,accuracy)

cols <- c("higgs"="#000000", "mnist"="#009E73", "cifar10"="#E69F00")

g <- ggplot(data=data.testBoth, aes(x=processTime, y=accuracy,color = dataset))
g <- g + geom_line(size=1.5)
g <- g + leg + labs(title="Error Rate vs Training Time", x="Training Time(s)", y="Error Rate", subtitle="500 trees, 1-40 depth")
g <- g + scale_color_manual(values=cols)


png(file="exp0047Both.png")
print(g)
dev.off()


