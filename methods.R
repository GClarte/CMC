



#noncompact version of CMC, without cell-list


CMCpasncomp <- function(x,f,eps){
  n=length(x)
  y=numeric(n)
  r=0
  for (i in 1:n){
    j=sample(1:n,1)
    yt=x[j]+runif(1,-eps,eps)
      alph=(sum(abs(x-x[i])<eps))/(sum(abs(x-yt)<eps)+1)*f(yt)/f(x[i])
      u=runif(1,0,1)
      if (u<alph){
        y[i]=yt
        r=r+1
      } else {
        y[i]=x[i]
      }
    }
  return(list(y,r))
}


toutncomp <- function(k,npart,f,eps){
  M=matrix(NA,nrow=npart,ncol=k)
  initialisation=runif(npart,0,1)
  M[,1]=initialisation
  accept=numeric(k-1)
  for (i in 2:k){
    O=CMCpas(M[,i-1],f,eps)
    M[,i]=O[[1]]
    accept[i-1]=O[[2]]
  }
  return(list(M,accept))
}

#compact version, with cell list


CMCpas <- function(x,cases,f,eps){
  n=length(x)
  y=numeric(n)
  casest=list()
  r=0
  for (i in 1:length(cases)){
    casest[[i]]=numeric()
  }
  for (i in 1:n){
    j=sample(1:n,1)
    yt=x[j]+runif(1,-eps,eps)
    if (yt>1 || yt<0){
      y[i]=x[i]
    } else {
      quellecaset=ceiling(yt/eps)
      voisinst=c(quellecaset-1,quellecaset+1,quellecaset)
      voisinst[voisinst==0]=length(cases)
      voisinst[voisinst==length(cases)+1]=1
      xvoist=c(cases[[voisinst[1]]],cases[[voisinst[3]]],cases[[voisinst[2]]])
      quellecase=ceiling(x[i]/eps)
      voisins=c(quellecase-1,quellecase+1,quellecase)
      voisins[voisins==0]=length(cases)
      voisins[voisins==length(cases)+1]=1
      xvois=c(cases[[voisins[1]]],cases[[voisins[3]]],cases[[voisins[2]]])
      alph=(sum(abs(xvois-x[i])<eps))/(sum(abs(xvoist-yt)<eps))*f(yt)/f(x[i])
      if (is.nan(alph)){
        alph=1/2
      }
      u=runif(1,0,1)
      if (u<alph){
        y[i]=yt
        r=r+1
        casest[[quellecaset]]=c(casest[[quellecaset]],y[i])
      } else {
        y[i]=x[i]
        casest[[quellecase]]=c(casest[[quellecase]],y[i])
      }
      
    }
  }
  return(list(y,casest,r))
}

#construit une initialisation uniforme
init <- function(k,eps){
  x=runif(k,0,1)
  ncases=ceiling(1/eps)
  l=list()
  for (i in 1:ncases)
  {
    l[[i]]=x[which(ceiling(x/eps)==i)]
  }
  return(list(x,l))
}

tout <- function(k,npart,f,eps){
  M=matrix(NA,nrow=npart,ncol=k)
  initialisation=init(npart,eps)
  M[,1]=initialisation[[1]]
  cases=initialisation[[2]]
  accept=numeric(k-1)
  for (i in 2:k){
    O=CMCpas(M[,i-1],cases,f,eps)
    M[,i]=O[[1]]
    cases=O[[2]]
    accept[i-1]=O[[3]]
  }
  return(list(M,accept))
}

CMCpaspar <- function(x,f,eps){
  n=length(x)
  y=numeric(n)
  p=numeric(n)
  x=sample(x)
  r=0
  for (i in 1:n){
    yt=x[i]+runif(1,-eps,eps)
    alph=f(yt)/f(x[i])
    if (is.nan(alph)){
      alph=.5
    }
    u=runif(1,0,1)
    if (u<alph){
      y[i]=yt
      r=r+1
    } else {
      y[i]=x[i]
    }
  }
  return(list(y,r))
}

toutpar <- function(k,npart,f,eps){
  n=npart
  xini=runif(npart,0,1)
  M=matrix(NA,nrow=n,ncol=k)
  M[,1]=xini
  accept=numeric(k-1)
  for (i in 2:k){
    O=CMCpaspar(M[,i-1],f,eps)
    M[,i]=O[[1]]
    accept[i-1]=O[[2]]
  }
  return(list(M,accept))
}


#PMC

pmcpasbis <- function(x,f,eps){
  y=rnorm(length(x),0,eps)
  xt=x + y
  pds=sapply(xt,f)/dnorm(y,0,eps)
  pds=pds/sum(pds)
  y=sample(xt,length(x),prob=pds,replace=T)
  return(y)
}

pmctotbis <- function(k,npart,f,eps){
  xini=runif(npart,0,1)
  M=matrix(NA,nrow=npart,ncol=k)
  M[,1]=xini
  for (i in 2:k){
    M[,i]=pmcpasbis(M[,i-1],f,eps)
  }
  return(M)
}
