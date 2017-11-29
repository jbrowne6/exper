require(meda)
load(file="ForJushua.RData")


dato <- as.matrix(ForJoshua2[,c(1:(ncol(ForJoshua2)-4), (ncol(ForJoshua2)-2):ncol(ForJoshua2))])

dat <- data.frame(scale(dato, center = TRUE, scale = FALSE))

truth<- as.numeric(ForJoshua2[,(ncol(ForJoshua2)-3)])+1
pdf(file="cancer.pdf", width=7,height=5) 

plot(mlocation(dato))
plot(d1heat(dat))
plot(outliers(dat))
plot(medacor(dat))
plot(cumvar(dat))
pairhex(dat)

h <- hmc(dat)
plot(h, pch = truth)
plotDend(h)
stackM(h, centered = TRUE, depth = h$dat$height)
stackM(h, centered = TRUE)
clusterMeans(h)

dev.off()

