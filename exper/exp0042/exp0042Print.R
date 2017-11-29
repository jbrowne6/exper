#---
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


load(file="accuracy.Rdata")

png(file="exp0042.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(x=pFac, y=Results)) + geom_point() +leg + labs(title="Error Rates vs P Factor", x="P Factor", y="Error Rate "))
dev.off()

