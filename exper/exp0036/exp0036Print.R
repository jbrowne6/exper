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

load(file="exp0036.Rdata")
load(file="exp0036Anum.Rdata")
lab <- labs(title="Forest Algorithm Strong Scaling", x="Number of Cores", y="Relative Performance", subtitle =paste("Dataset Size: Higgs(250000x31)"))

png(file="exp0036.png", width=pWidth, height=pHeight)
print(ggplot(ress1,aes(x=Cores_Used, y=Speed_Up, group=Line_Type, color=Line_Type))+geom_line()+lab +leg + geom_text(x = tWidth, y =tHeight , label =paste("Amdahl Number\nRerF: ",signif(amCal[2,2], digits=2),"\nXGBoost: ", signif(amCal[4,2], digits=2), "\nRanger: ", signif(amCal[5,2],digits=2))))
dev.off()
