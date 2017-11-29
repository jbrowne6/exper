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

load(file="size.Rdata")

cols <- c("Ideal"="#000000", "RerF"="#009E73", "XGBoost"="#E69F00", "Ranger"="#0072B2", "RF"="#CC79A7")

png(file="exp0029.png", width=pWidth, height=pHeight)
print(g <- ggplot(ress1, aes(Dataset, value)) + geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ leg + labs(title="Average Tree Size for Dataset", x="Dataset", y="Average Tree Size (Mb)")+ scale_fill_manual(values=cols))
dev.off()
