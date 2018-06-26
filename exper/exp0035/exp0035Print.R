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
nTimes <- 10
run_su <- TRUE
leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))


load(file="exp0035.Rdata")


lab <- labs(title=paste("Median Per Tree Training Time vs\nNumber of Threads"), x="Number of Threads", y="Time (s)", subtitle =paste("Dataset Size: p53 (31159 x 5409)"))


png(file="exp0035.png", width=pWidth, height=pHeight)
print(ggplot(ress1,aes(x=Cores_Used, y=Time_Sec, group=Line_Type, color=Line_Type))+geom_line()+lab +leg)
dev.off()


