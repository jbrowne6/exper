library(ggplot2)
leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

load(file="testTime3.Rdata")
#ress1 <- ress1[3:10,]
ress1[,2]<- as.factor(ress1[,2])
ress1[,3]<- as.numeric(ress1[,3])

p <- ggplot(ress1, aes(Dataset, value,color = variable, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+ leg + labs(title="Test Times Single Core", x="Dataset", y="Test Time (s) ", subtitle=paste(""))

png(file="exp0045a.png")
print(p)
dev.off()


