source("testtargets.R")
source("methods.R")

library(parallel)
library(ggplot2)

test <- function(npart,eps,nbiter,nbrep,fex,max){
  sam=sampletest(npart,fex,max)
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=tout(nbiter,npart,fex,eps)[[1]];return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

testpar<- function(npart,eps,nbiter,nbrep,fex,max){
  sam=sampletest(npart,fex,max)
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=toutpar(nbiter,npart,fex,eps)[[1]];return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

testpmc <- function(npart,eps,nbiter,nbrep,fex,max){
  sam=sampletest(npart,fex,max)
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=pmctotbis(nbiter,npart,fex,eps);return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

#variations for each parameter
v=1:3
w=1:5
z=1:5
fonctions=list(ftest1,ftest2,ftest3)
max=c(max1,max2,max3)
epspar=c(1,0.5,0.1,0.05,0.01)
epscmc=c(1/4,1/3,1/2,3/4,1)
npart=c(1000,2500,5000,7500,10000)
CMCres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){test(npart[yy],1/(npart[yy]^epscmc[y]),50,50,fonctions[[x]],max[x])})})})
Parres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){testpar(npart[yy],epspar[[y]],50,50,fonctions[[x]],max[x])})})})
PMCres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){testpmc(npart[yy],epspar[[y]],50,50,fonctions[[x]],max[x])})})})

#choice of N
z=1:5
npart=c(1000,5000,10000,50000,100000)
CMCres3=mclapply(npart,function(x){test(x,x^(-1/2),50,50,ftest3,max3)})
CMCres2=mclapply(npart,function(x){test(x,x^(-1/2),50,50,ftest2,max2)})


CMCres3.3=mclapply(npart,function(x){test(x,x^(-1/3),50,50,ftest3,max3)})
CMCres2.3=mclapply(npart,function(x){test(x,x^(-1/3),50,50,ftest2,max2)})


CMCres3.1=mclapply(npart,function(x){test(x,x^(-1),50,50,ftest3,max3)})
CMCres2.1=mclapply(npart,function(x){test(x,x^(-1),50,50,ftest2,max2)})



#plots
moyenneCMCb <- numeric()

for (j in c(2,4)){
moyenneCMCb <- c(moyenneCMCb,CMCres2[[j]][,2],CMCres3[[j]][,2],CMCres2.3[[j]][,2],CMCres3.3[[j]][,2],CMCres2.1[[j]][,2],CMCres3.1[[j]][,2])
}

Function=rep(c(rep("2",50),rep("3",50)),3*5)
Iteration=rep(1:50,6*5)
npart=c(rep("100",6*50),rep("500",6*50),rep("1000",6*50),rep("5000",6*50),rep("10000",6*50))
alpha=rep(c(rep("1/2",100),rep("1/3",100),rep("1",100)),5)

pl=data.frame(KS=moyenneCMCb,Function=Function,Npart=npart,Alpha=alpha,Iteration=Iteration)
pl=pl[pl$Alpha=="1/2",]
pl$Npart <- factor(pl$Npart, levels=c("100", "500", "1000","5000","10000"))

p <- ggplot(data=pl,aes(x=Iteration,y=KS)) + geom_line(aes(color=Npart)) + facet_grid(cols = vars(Function),rows=vars(Alpha),scales="free_y")
p <- p+theme(panel.background = element_rect(fill = 'white', colour = 'grey'),legend.position="bottom")
p <- p+scale_color_viridis_d()
ggsave("distancenpartfini.pdf",height=7,width=10,unit="cm")

#avec un truc que PMC aime partout
P=c(CMCres[[1]][[3]][[5]][,2],Parres[[1]][[4]][[5]][,2],PMCres[[1]][[2]][[5]][,2],CMCres[[2]][[3]][[5]][,2],Parres[[2]][[4]][[5]][,2],PMCres[[2]][[2]][[5]][,2],CMCres[[3]][[3]][[5]][,2],Parres[[3]][[4]][[5]][,2],PMCres[[3]][[2]][[5]][,2])
Iter=rep(1:50,9)
Functions=c(rep("1",150),rep("2",150),rep("3",150))
Method=rep(c(rep("Collective",50),rep("Parallel",50),rep("Population",50)),3)
Q=data.frame(KS=P,Iterations=Iter,Functions=Functions,Method=Method)
p=ggplot(data=Q,aes(x=Iterations,y=KS)) + geom_line(aes(linetype=Method)) +theme(panel.background = element_rect(fill = 'white', colour = 'grey'),axis.title.x =element_blank())
p = p+facet_grid(cols=vars(Functions))
ggsave("distance10kpmc1.pdf",height=6,width=18, unit="cm")

#on fait varier epsilon dans collectif uniquement
P=c(CMCres[[1]][[1]][[5]][,2],CMCres[[1]][[2]][[5]][,2],CMCres[[1]][[3]][[5]][,2],CMCres[[1]][[4]][[5]][,2],CMCres[[1]][[5]][[5]][,2],
    CMCres[[2]][[1]][[5]][,2],CMCres[[2]][[2]][[5]][,2],CMCres[[2]][[3]][[5]][,2],CMCres[[2]][[4]][[5]][,2],CMCres[[2]][[5]][[5]][,2],
    CMCres[[3]][[1]][[5]][,2],CMCres[[3]][[2]][[5]][,2],CMCres[[3]][[3]][[5]][,2],CMCres[[3]][[4]][[5]][,2],CMCres[[3]][[5]][[5]][,2])
Iter=rep(1:50,15)
Function=c(rep("1",250),rep("2",250),rep("3",250))
Epsilon=rep(c(rep("1/4",50),rep("1/3",50),rep("1/2",50),rep("3/4",50),rep("1",50)),3)
Q=data.frame(KS=P,Iteration=Iter,Function=Function,alpha=Epsilon)
p=ggplot(data=Q,aes(x=Iter,y=KS))+ geom_line(aes(color=alpha)) +theme(panel.background = element_rect(fill = 'white', colour = 'grey'),axis.title.x =element_blank())
p=p+facet_grid(cols=vars(Function))
ggsave("distanceepsilon.pdf",height=6,width=18,unit="cm")


