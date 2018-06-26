# ---
# title: "Test speed-up and scale-up of R-Rerf against xg and ranger"
# author: "James Browne"
# date: "Oct 10 2017"
# output: html_document
# ---
library(ggplot2)
library(plyr)

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


mydata <- read.csv(file="figure4.csv", header=FALSE)
data_summary(mydata,varname="V4",groupnames=c("V1","V2"))
mydata <- data_summary(mydata,varname="V4",groupnames=c("V1","V2"))
mydata$V1 <- as.factor(mydata$V1)



p <- ggplot(mydata, aes(x=V1, y=V4, fill=supp)) + 
       geom_bar(stat="identity", position=position_dodge()) +
         geom_errorbar(aes(ymin=V4-sd, ymax=V4+sd), width=.2,
                                        position=position_dodge(.9))
  
p + scale_fill_brewer(palette="Paired") + theme_minimal()


pWidth = 300
pHeight = 300





png(file="figure4.png", width=pWidth, height=pHeight)
print(p)
dev.off()

