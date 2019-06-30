source("testtargets.R")
source("methods.R")
library(ggplot2)

#gaussians
U1=tout(100,10000,ftest1,0.01)
V1=toutpar(100,10000,ftest1,0.01)
W1=pmctot(100,10000,ftest1,0.5)

U2=tout(100,10000,ftest2,0.01)
V2=toutpar(100,10000,ftest2,0.01)
W2=pmctot(100,10000,ftest2,0.5)

U3=tout(100,10000,ftest3,0.01)
V3=toutpar(100,10000,ftest3,0.01)
W3=pmctot(100,10000,ftest3,0.5)

x=seq(0,1,.01)
Vrai1=sapply(x,ftest1)
Vrai2=sapply(x,ftest2)
Vrai3=sapply(x,ftest3)
Vrai1=Vrai1/sum(Vrai1*0.01)
Vrai2=Vrai2/sum(Vrai2*0.01)
Vrai3=Vrai3/sum(Vrai3*0.01)

Vrai=data.frame(pos=rep(x,3),Density=c(Vrai1,Vrai2,Vrai3),Function=c(rep("1",length(x)),rep("2",length(x)),rep("3",length(x))))


Value = c(U1[[1]][,100],U2[[1]][,100],U3[[1]][,100],V1[[1]][,100],V2[[1]][,100],V3[[1]][,100],W1[,100],W2[,100],W3[,100])
Method=c(rep("Collective",30000),rep("Parallel",30000),rep("Population",30000))
Function=rep(c(rep("1",10000),rep("2",10000),rep("3",10000)),3)
pl=data.frame(Value=Value,Method=Method,Function=Function)
p1 <- ggplot(data=pl[1:30000,])  + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density))+ facet_grid(cols=vars(Method),rows=vars(Function),scales="free") +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
p2 <- ggplot(data=pl[30001:60000,]) + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density)) + facet_grid(cols=vars(Method),rows=vars(Function),scales="free")+  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
p3 <- ggplot(data=pl[60001:90000,]) + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density)) + facet_grid(cols=vars(Method),rows=vars(Function),scales="free")+  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
ggarrange(p1,p2,p3,ncol=3)
ggsave("plotT100.pdf",height=8,width=16,unit="cm")

#cauchy

U4b=toutncomp(100,10000,ftest4ncomp,0.05)
V4b=toutpar(100,10000,ftest4ncomp,0.05)
W4b=pmctot(100,10000,ftest4ncomp,0.5)

U5b=toutncomp(100,10000,ftest5ncomp,0.05)
V5b=toutpar(100,10000,ftest5ncomp,0.05)
W5b=pmctot(100,10000,ftest5ncomp,0.5)


x=seq(-.5,1.5,.01)
Vrai1=sapply(x,ftest4ncomp)/2
Vrai2=sapply(x,ftest5ncomp)/2

Vrai=data.frame(pos=rep(x,2),Density=c(Vrai1,Vrai2),Function=c(rep("4",length(x)),rep("5",length(x))))

Value = c(U4b[[1]][,100],U5b[[1]][,100],V4b[[1]][,100],V5b[[1]][,100],W4b[,100],W5b[,100])
Method=c(rep("Collective",20000),rep("Parallel",20000),rep("Population",20000))
Function=rep(c(rep("4",10000),rep("5",10000)),3)
pl=data.frame(Value=Value,Method=Method,Function=Function)
p1 <- ggplot(data=pl[1:20000,]) + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density)) + facet_grid(cols=vars(Method),rows=vars(Function),scales="free") +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
p2 <- ggplot(data=pl[20001:40000,]) + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density))+ facet_grid(cols=vars(Method),rows=vars(Function),scales="free")+  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
p3 <- ggplot(data=pl[40001:60000,]) + geom_histogram(aes(x=Value,y=..density..),bins=100) + geom_line(data=Vrai,aes(x=pos,y=Density))+ facet_grid(cols=vars(Method),rows=vars(Function),scales="free")+  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x =element_blank())
ggarrange(p1,p2,p3,ncol=3)
ggsave("plotT100cauchyncomp.pdf",height=6,width=16,unit="cm")
