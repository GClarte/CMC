#samples by accept-reject

sampletest <- function(k,ftest,max){
  u=numeric(k)
  for (i in 1:k){
    u[i]=runif(1,0,1)
    t=runif(1,0,max)
    while(t>ftest(u[i])){
      u[i]=runif(1,0,1)
      t=runif(1,0,max)
    }}
  return(u)
}

#fonctions pour les tests
#gaussienne gentille
ftest1 <- function(x){
  if (x>1 || x<0){
    return(0)
  } else {
    return(dnorm(x,.6,.08))
  }
}
max1=5

#mélange de gaussienne gentilles
ftest2 <- function(x){
  if(x>1 || x<0){
    return(0)
  } else {
    return(0.07*dnorm(x,.25,.16) + 0.2*dnorm(x,.8,.05))
  }
}
max2=1.2

#spires in the mist
ftest3 <- function(x){
  if(x>1 || x<0){
    return(0)
  } else {
    return(.7*dnorm(x,.72,.3)+.5*dnorm(x,.3,.2)+20*dnorm(x,.8,.002)+100*dnorm(x,.6,.002)+50*dnorm(x,.34,.002)+200*dnorm(x,.1,.002))
  }
}
max3=400000

#cauchy gaussian assez écartées

ftest4ncomp <- function(x){
    return(dcauchy(x,.1,.01) + dnorm(x,.9,.01))
}
max4=40

#double cauchy


ftest5ncomp <- function(x){
    return(dcauchy(x,.1,0.1)+dcauchy(x,.8,.05))
}
max5=10



#versions PAWL

eps=.05

rinit <- function(size) runif(size,0,1)
parameters=list()
pawltarget1 <- target(name = "gaussian", dimension = 1,rinit = rinit, logdensity = function(x,parameters){log(ftest1(x))},parameters = parameters)
pawltarget2 <- target(name = "gaussian", dimension = 1,rinit = rinit, logdensity = function(x,parameters){log(ftest2(x))},parameters = parameters)
pawltarget3 <- target(name = "gaussian", dimension = 1,rinit = rinit, logdensity = function(x,parameters){log(ftest3(x))},parameters = parameters)
pawltarget4 <- target(name = "gaussian", dimension = 1,rinit = rinit, logdensity = function(x,parameters){log(ftest4ncomp(x))},parameters = parameters)
pawltarget5 <- target(name = "gaussian", dimension = 1,rinit = rinit, logdensity = function(x,parameters){log(ftest5ncomp(x))},parameters = parameters)
getPos <- function(points, logdensity) points
proposal=createAdaptiveRandomWalkProposal(10000, 1, F, sigma_init=eps)
binrange=c(0,1)
energybinning <- binning(position = getPos,name = "energy",binrange = binrange,ncuts = 2,useLearningRate = FALSE,autobinning = FALSE)
pawlparameters <- tuningparameters(nchains = 10000, niterations = 50, storeall = TRUE)
pawlresults <- pawl(pawltarget1, binning = energybinning, AP = pawlparameters,proposal = proposal)
