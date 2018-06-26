pWidth = 800
pHeight = 800
tWidth = pWidth * .05
tHeight = .13 * pHeight


library(ggplot2)
load(file="1000.Rdata")
load(file="1000p.Rdata")
load(file="1000RL.Rdata")
load(file="1000RLp.Rdata")
load(file="1000RS.Rdata")
load(file="1000RSp.Rdata")
ress1 <- rbind(re1000,re1000p,re1000RL,re1000RLp, re1000RS, re1000RSp)

leg <- theme(legend.text = element_text(size = 12), legend.title=element_blank(), plot.title=element_text(size = 16,  face="bold"), plot.subtitle = element_text(size = 12),axis.title.x =element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

lab <- labs(title="Specificity vs Sensitivity")
png(file="together.png", width=pWidth, height=pHeight)
print(ggplot(ress1, aes(x=specificity, y = sensitivity, colour=type)) + geom_line(aes(linetype=type)) + leg + lab + ylim(.5, 1) + geom_point(aes(shape=type)) )
#print(ggplot(ress1, aes(x=specificity, y = sensitivity, colour=type)) + geom_line() + leg + lab)
dev.off()

