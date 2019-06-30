source("testtargets.R")
source("methods.R")

library(parallel)
#estimation de distances pour une taille de fenêtre et nb de particules
testncomp <- function(npart,eps,nbiter,nbrep,fex,max,sam){
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=toutncomp(nbiter,npart,fex,eps)[[1]];return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

testpar<- function(npart,eps,nbiter,nbrep,fex,max,sam){
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=toutpar(nbiter,npart,fex,eps)[[1]];return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

testpmc <- function(npart,eps,nbiter,nbrep,fex,max,sam){
  v=rep(1,nbrep)
  Q=sapply(v,function(u){V=pmctotbis(nbiter,npart,fex,eps);return(apply(V,2,function(x){ks.test(x,sam)$statistic}))})
  return(cbind(apply(Q,1,median),apply(Q,1,mean),apply(Q,1,var)))
}

#on teste avec plusieurs taille de fenêtres et plusieurs fonctions, 5000 part à chaque fois

v=1:2
w=1:5
z=1
fonctions=list(ftest4ncomp,ftest5ncomp)
samf=list(function(x){(pcauchy(x,.1,.01) + pnorm(x,.9,.01))/2},function(x){(pcauchy(x,.1,0.1)+pcauchy(x,.8,.05))/2})
epspar=c(1,0.5,0.1,0.05,0.01)
epscmc=c(1/4,1/3,1/2,3/4,1)
npart=c(10000)
CMCres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){testncomp(npart[yy],1/(npart[yy]^epscmc[y]),50,50,fonctions[[x]],max[x],samf[[x]])})})})
Parres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){testpar(npart[yy],epspar[[y]],50,50,fonctions[[x]],max[x],samf[[x]])})})})
PMCres=mclapply(v,function(x){mclapply(w,function(y){mclapply(z,function(yy){testpmc(npart[yy],epspar[[y]],50,50,fonctions[[x]],max[x],samf[[x]])})})})


#plots

moyenneCMC <- numeric()
moyennePar <- numeric()
moyennePMC <- numeric()
for (l in 1:2){
  for (j in 1:5){
      moyenneCMC <- c(moyenneCMC,CMCres[[l]][[j]][[1]][,2])
      moyennePar <- c(moyennePar,Parres[[l]][[j]][[1]][,2])
      moyennePMC <- c(moyennePMC,PMCres[[l]][[j]][[1]][,2])
  }
}

P=c(CMCres[[1]][[2]][[1]][,2],Parres[[1]][[3]][[1]][,2],PMCres[[1]][[2]][[1]][,2],CMCres[[2]][[2]][[1]][,2],Parres[[2]][[3]][[1]][,2],PMCres[[2]][[2]][[1]][,2])
Iter=rep(1:50,6)
Function=c(rep("4",150),rep("5",150))
Type=rep(c(rep("Collective",50),rep("Parallel",50),rep("Population",50)))
Q=data.frame(KS=P,Iteration=Iter,Function=Function,Method=Type)
p=ggplot(data=Q,aes(x=Iter,y=KS))+ geom_line(aes(linetype=Method)) +theme(panel.background = element_rect(fill = 'white', colour = 'grey'),axis.title.x =element_blank())
p=p+facet_grid(cols=vars(Function))

ggsave("distancecauchymoins.pdf",height=6,width=15,unit="cm")
