# ---
# title: "Test speed-up and scale-up of R-Rerf against xg and ranger"
# author: "James Browne"
# date: "Oct 10 2017"
# output: html_document
# ---

pWidth = 300
pHeight = 300
tWidth = pWidth * .05
tHeight = .13 * pHeight

library(ggplot2)
leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

load(file="testTime.Rdata")



#ggsave(
#       "exp0038.png",
#ggplot(ress1, aes(Dataset, value,color = variable, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+ leg + labs(title="Test Times Single Core", x="Dataset", y="Test Time (s) "),#, subtitle=paste(""))
#width= 2.0,
#height = 2.0,
#dpi = 1200
#)


png(file="exp0038.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(Dataset, value,color = variable, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+ leg + labs(title="Test Times Single Core", x="Dataset", y="Test Time (s) "))#, subtitle=paste("")))
dev.off()

