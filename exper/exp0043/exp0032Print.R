#---
# title: "Test speed-up and scale-up of R-Rerf against xg and ranger"
# author: "James Browne"
# date: "Oct 10 2017"
# output: html_document
# ---
pWidth = 600
pHeight = 600


library(ggplot2)
leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))


load(file="accuracy.Rdata")

png(file="exp0043.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(Dataset, value,color = variable)) + geom_point(position=position_jitterdodge(dodge.width=0.5))+ leg + labs(title="Error Rates", x="Dataset", y="Average Error Rate ", subtitle=paste("")))
dev.off()

