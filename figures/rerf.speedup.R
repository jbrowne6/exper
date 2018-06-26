pWidth = 300
pHeight = 300
tWidth = pWidth * .05
tHeight = .13 * pHeight



library(ggplot2)
#library(gridExtra)
leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title = element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))


load(file="../exper/exp0027/exp0027.Rdata")
ds <- cbind(dataset = "MNIST (60000x784)", ress1)

load(file="../exper/exp0036/exp0036.Rdata")
ds <- rbind(ds, cbind(dataset = "Higgs(250000x31)", ress1))

load(file="../exper/exp0037/exp0037.Rdata")
ds <- rbind(ds, cbind(dataset = "p53(31159x5409)", ress1))

lab <- labs(title="Forest Algorithm Strong Scaling", x="Number of Threads", y="Relative Performance")

cols <- c("Ideal"="#000000", "RerF"="#009E73", "XGBoost"="#E69F00", "Ranger"="#0072B2", "RF"="#CC79A7")

p <- ggplot(ds,aes(x=Cores_Used, y=Speed_Up, group=Line_Type, color=Line_Type))+geom_line()+lab +leg + facet_grid(. ~ dataset)+ scale_x_continuous(breaks = c(0,10,20,30,40)) + scale_color_manual(values=cols)
png(file="rerf.speedup.png", width=7.5, height=2.5, units='in', res=600)
print(p)
dev.off()

