
source("testtargets.R")
source("methods.R")
library(ggplot2)


U1k.5=colMeans(tout(200,1000,ftest1,1/20)[[1]])
U1k1=colMeans(tout(200,1000,ftest1,1/1000)[[1]])
U1k.25=colMeans(tout(200,1000,ftest1,1/(1000)^(.25))[[1]])

U5k.5=colMeans(tout(200,5000,ftest1,1/5000^.5)[[1]])
U5k1=colMeans(tout(200,5000,ftest1,1/5000)[[1]])
U5k.25=colMeans(tout(200,5000,ftest1,1/(5000)^(.25))[[1]])

U10k.5=colMeans(tout(200,10000,ftest1,1/10000^.5)[[1]])
U10k1=colMeans(tout(200,10000,ftest1,1/10000)[[1]])
U10k.25=colMeans(tout(200,10000,ftest1,1/(10000)^(.25))[[1]])

U100k.5=colMeans(tout(200,100000,ftest1,1/100000^.5)[[1]])
U100k1=colMeans(tout(200,100000,ftest1,1/100000)[[1]])
U100k.25=colMeans(tout(200,100000,ftest1,1/(100000)^(.25))[[1]])

V1k.5=colMeans(toutpar(200,1000,ftest1,1/20)[[1]])
V1k1=colMeans(toutpar(200,1000,ftest1,1/1000)[[1]])
V1k.25=colMeans(toutpar(200,1000,ftest1,1/(1000)^(.25))[[1]])

V5k.5=colMeans(toutpar(200,5000,ftest1,1/5000^.5)[[1]])
V5k1=colMeans(toutpar(200,5000,ftest1,1/5000)[[1]])
V5k.25=colMeans(toutpar(200,5000,ftest1,1/(5000)^(.25))[[1]])

V10k.5=colMeans(toutpar(200,10000,ftest1,1/10000^.5)[[1]])
V10k1=colMeans(toutpar(200,10000,ftest1,1/10000)[[1]])
V10k.25=colMeans(toutpar(200,10000,ftest1,1/(10000)^(.25))[[1]])

W1k.5=colMeans(pmctot(200,1000,ftest1,.5))
W1k1=colMeans(pmctot(200,1000,ftest1,1))

W5k.5=colMeans(pmctot(200,5000,ftest1,.5))
W5k1=colMeans(pmctot(200,5000,ftest1,1))

W10k.5=colMeans(pmctot(200,10000,ftest1,.5))
W10k1=colMeans(pmctot(200,10000,ftest1,1))

W100k.5=colMeans(pmctot(200,100000,ftest1,.5))
W100k1=colMeans(pmctot(200,100000,ftest1,1))

#plots

FFn=list(U10k.5,U10k1,V10k.5,V10k1)

acf=unlist(lapply(FFn,function(x){acf(x)$acf}))
lags=rep(1:24,4)
type=c(rep("Collective",24*2),rep("Parallel",24*2))
eps=rep(c(rep(".5",24),rep("1",24)),4)
pourplot=data.frame(ACF=acf,Method=type,Epsilon=eps,Lag=lags)

p <- ggplot(data=pourplot,aes(x=Lag,y=ACF))+ geom_line(aes(linetype=Method)) + facet_grid(cols = vars(Epsilon))
p <- p+theme(panel.background = element_rect(fill = 'white', colour = 'grey'),legend.position="bottom")
p <- p + geom_hline(yintercept = 0,alpha=.5)
ggsave("acffct1.pdf",width = 12,height=6,units="cm")

